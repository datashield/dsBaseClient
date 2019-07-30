#' 
#' @title ds.var.o calling aggregate function varDS.o
#' @description Computes the variance of a given vector
#' This function is similar to the R function \code{var}.
#' @details It is a wrapper for the server side function. 
#' The server side function returns a list with the sum of the input variable, the sum of squares
#' of the input variable, the number of missing values, the number of valid values, the number of
#' total lenght of the variable, and a study message indicating whether the number of valid is less
#' than the disclosure threshold. The variance is calculated at the client side by the formula 
#' $\deqn{var(X)}{\frac{\sum{x_i^2}}{N-1}-\frac{(\sum{x_i})^2}{N(N-1)}}$
#' @param x a character, the name of a numerical vector.
#' @param type a character which represents the type of analysis to carry out. 
#' If \code{type} is set to 'combine', 'combined', 'combines' or 'c', a global variance is calculated 
#' if \code{type} is set to 'split', 'splits' or 's', the variance is calculated separately for each study.
#' if \code{type} is set to 'both' or 'b', both sets of outputs are produced
#' @param checks a Boolean indicator of whether to undertake optional checks of model
#' components. Defaults to checks=FALSE to save time. It is suggested that checks
#' should only be undertaken once the function call has failed
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @return a list including: Variance.by.Study = estimated variance in each study separately (if type = split or both), with Nmissing
#' (number of missing observations), Nvalid (number of valid observations), Ntotal (sum of missing and valid observations)
#' also reported separately for each study; Global.Variance = Variance, Nmissing, Nvalid, Ntotal across all studies combined
#' (if type = combine or both); Nstudies = number of studies being analysed; ValidityMessage indicates whether 
#' a full analysis was possible or whether one or more studies had fewer valid observations than the nfilter
#' threshold for the minimum cell size in a contingency table.
#' @author Amadou Gaye, Demetris Avraam, for DataSHIELD Development Team
#' @export
#' @examples
#' \dontrun{
#' 
#'   # load that contains the login details
#'   data(logindata)
#' 
#'   # login and assign specific variable(s)
#'   myvar <- list('LAB_TSC')
#'   opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#'   # Example 1: compute the pooled variance of the variable 'LAB_TSC' - default behaviour
#'   ds.var(x='D$LAB_TSC')
#' 
#'   # Example 2: compute the variance of each study separately
#'   ds.var(x='D$LAB_TSC', type='split')
#' 
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(opals)
#' 
#' }
#' 
ds.var.o <- function(x=NULL, type='split', checks=FALSE, datasources=NULL){
  
  #####################################################################################
  #MODULE 1: IDENTIFY DEFAULT OPALS  											                        		#
  # if no opal login details are provided look for 'opal' objects in the environment  #
  if(is.null(datasources)){														                               	#
    datasources <- findLoginObjects()							                          					#
  }																                                          					#						
  #####################################################################################  
  
  #####################################################################################
  #MODULE 2: SET UP KEY VARIABLES ALLOWING FOR DIFFERENT INPUT FORMATS                #
  if(is.null(x)){                                                                   #
    stop("Please provide the name of the input vector!", call.=FALSE)               #
  }                                                                                 #
  # the input variable might be given as a variable in a data frame (i.e. D$x)      #
  # or just as a vector not attached to a table (i.e. x)                            #
  # we have to make sure the function deals with each case                          #
  xnames <- extract(x)                                                              #
  varname <- xnames$elements                                                        #
  obj2lookfor <- xnames$holders                                                     #
  #####################################################################################
  
  ###############################################################################################
  #MODULE 3: GENERIC OPTIONAL CHECKS TO ENSURE CONSISTENT STRUCTURE OF KEY VARIABLES            #
  #IN DIFFERENT SOURCES                                                                         #
  # beginning of optional checks - the process stops and reports as soon as one               #
  #check fails                                                                                #
  #
  if(checks){                                                                                 #
    message(" -- Verifying the variables in the model")                                       #
    #
    # check if the input object(s) is(are) defined in all the studies                           #
    if(is.na(obj2lookfor)){                                                                     #
      defined <- isDefined(datasources, varname)                                                #
    }else{                                                                                      #
      defined <- isDefined(datasources, obj2lookfor)                                            #
    }                                                                                           #
    #
    # call the internal function that checks the input object is suitable in all studies        #
    varClass <- checkClass(datasources, x)                                                      #
    # the input object must be a numeric or an integer vector                                   #
    if(varClass != 'integer' & varClass != 'numeric'){                                          #
      stop("The input object must be an integer or a numeric vector.", call.=FALSE)             #
    }                                                                                           #
  }                                                                                             #
  ###############################################################################################
  
  ###################################################################################################
  #MODULE 4: EXTEND "type" argument to include "both" and enable valid alisases                     #
  if(type == 'combine' | type == 'combined' | type == 'combines' | type == 'c') type <- 'combine'   #
  if(type == 'split' | type == 'splits' | type == 's') type <- 'split'                              #
  if(type == 'both' | type == 'b' ) type <- 'both'                                                  #
  #
  #MODIFY FUNCTION CODE TO DEAL WITH ALL THREE TYPES                                                #
  ###################################################################################################
  
  cally <- paste0("varDS.o(", x, ")")
  ss.obj <- opal::datashield.aggregate(datasources, as.symbol(cally))
  
  Nstudies <- length(datasources)
  EstimatedVar <- c()
  Nvalid <- c()
  Nmissing <- c()
  Ntotal <- c()
  for (i in 1:Nstudies){
    EstimatedVar[i] <- ss.obj[[i]][[2]]/(ss.obj[[i]][[4]]-1) - (ss.obj[[i]][[1]])^2/(ss.obj[[i]][[4]]*(ss.obj[[i]][[4]]-1))
    Nvalid[i] <- as.numeric(ss.obj[[i]][[4]])
    Nmissing[i] <- as.numeric(ss.obj[[i]][[3]])
    Ntotal[i] <- as.numeric(ss.obj[[i]][[5]])
  }
  ss.mat <- matrix(c(EstimatedVar,Nmissing,Nvalid,Ntotal),nrow=Nstudies)
  dimnames(ss.mat) <- c(list(names(ss.obj),c('EstimatedVar','Nmissing','Nvalid','Ntotal')))
  
  ValidityMessage.mat <- matrix(matrix(unlist(ss.obj),nrow=Nstudies,byrow=TRUE)[,6],nrow=Nstudies)
  dimnames(ValidityMessage.mat) <- c(list(names(ss.obj),names(ss.obj[[1]])[6]))
  
  ss.mat.combined <- t(matrix(ss.mat[1,]))
  
  GlobalSum.new <- 0
  GlobalSumSquares.new <- 0
  GlobalNvalid.new <- 0
  for (i in 1:Nstudies){
    GlobalSum <- GlobalSum.new +  ss.obj[[i]][[1]]
    GlobalSumSquares <- GlobalSumSquares.new +  ss.obj[[i]][[2]]
    GlobalNvalid <- GlobalNvalid.new +  ss.obj[[i]][[4]]
    GlobalSum.new <- GlobalSum
    GlobalSumSquares.new <- GlobalSumSquares
    GlobalNvalid.new <- GlobalNvalid
  }
  
  GlobalVar <- GlobalSumSquares/(GlobalNvalid-1) - (GlobalSum^2)/(GlobalNvalid*(GlobalNvalid-1))
  
  
  ss.mat.combined[1,1] <- GlobalVar
  ss.mat.combined[1,2] <- sum(ss.mat[,2])
  ss.mat.combined[1,3] <- sum(ss.mat[,3])
  ss.mat.combined[1,4] <- sum(ss.mat[,4])

  
  dimnames(ss.mat.combined) <- c(list("studiesCombined",c('EstimatedVar','Nmissing','Nvalid','Ntotal')))
  
  #PRIMARY FUNCTION OUTPUT SUMMARISE RESULTS FROM
  #AGGREGATE FUNCTION AND RETURN TO CLIENT-SIDE
  if (type=='split'){
    return(list(Variance.by.Study=ss.mat,Nstudies=Nstudies,ValidityMessage=ValidityMessage.mat))
  }
  
  if (type=="combine"){
    return(list(Global.Variance=ss.mat.combined,Nstudies=Nstudies,ValidityMessage=ValidityMessage.mat))
  }
  
  if (type=="both"){
    return(list(Variance.by.Study=ss.mat,Global.Variance=ss.mat.combined,Nstudies=Nstudies,ValidityMessage=ValidityMessage.mat))
  }
 
}
#ds.var.o
