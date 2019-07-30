#' 
#' @title Computes the statistical mean of a given vector
#' @description This function is similar to the R function \code{mean}.
#' @details It is a wrapper for the server side function.
#' @param x a character, typically the name of a numerical vector
#' @param type a character which represents the type of analysis to carry out. 
#' If \code{type} is set to 'combine', 'combined', 'combines' or 'c', a global mean is calculated 
#' if \code{type} is set to 'split', 'splits' or 's', the mean is calculated separately for each study.
#' if \code{type} is set to 'both' or 'b', both sets of outputs are produced
#' @param checks a Boolean indicator of whether to undertake optional checks of model
#' components. Defaults to checks=FALSE to save time. It is suggested that checks
#' should only be undertaken once the function call has failed
#' @param save.mean.Nvalid a Boolean indicator of whether the user wishes to save the
#' generated values of the mean and of the number of valid (non-missing) observations into
#' the R environments at each of the data servers. Will save study-specific means and Nvalids
#' as well as the global equivalents across all studies combined. Once the estimated means and Nvalids
#' are written into the server-side R environments, they can be used directly to centralize 
#' the variable of interest around its global mean or its study-specific means. Finally,
#' the isDefined internal function checks whether the key variables have been created.
#' @param datasources specifies the particular opal object(s) to use, if it is not specified
#' the default set of opals will be used. The default opals are always called default.opals.
#' This parameter is set without inverted commas: e.g. datasources=opals.em or datasources=default.opals
#' If you wish to specify the second opal server in a set of three, the parameter is specified:
#' e.g. datasources=opals.em[2]. If you wish to specify the first and third opal servers in a set specify:
#' e.g. datasources=opals.em[2,3]
#' @return a list including: Mean.by.Study = estimated mean in each study separately (if type = split or both), with Nmissing
#' (number of missing observations), Nvalid (number of valid observations), Ntotal (sum of missing and valid observations)
#' also reported separately for each study; Global.Mean = Mean, Nmissing, Nvalid, Ntotal across all studies combined
#' (if type = combine or both); Nstudies = number of studies being analysed; ValidityMessage indicates whether 
#' a full analysis was possible or whether one or more studies had fewer valid observations than the nfilter
#' threshold for the minimum cell size in a contingency table. If save.mean.Nvalid=TRUE, ds.mean.o writes
#' the objects "Nvalid.all.studies", "Nvalid.study.specific", "mean.all.studies", and "mean.study.specific"
#' to the serverside on each server 
#' @author Burton PR; Gaye A; Isaeva I;
#' @seealso \code{ds.quantileMean} to compute quantiles.
#' @seealso \code{ds.summary} to generate the summary of a variable.
#' @export
#' @examples
#' \dontrun{
#' 
#' #  # load that contains the login details
#' #  data(logindata)
#' #  library(opal)
#' #
#' #  # login and assign specific variable(s)
#' #  myvar <- list('LAB_TSC')
#' #  opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' #
#' #  # Example 1: compute the pooled statistical mean of the variable 'LAB_TSC' - default behaviour
#' #  ds.mean(x='D$LAB_TSC')
#' #
#' #  # Example 2: compute the statistical mean of each study separately
#' #  ds.mean(x='D$LAB_TSC', type='split')
#' #
#' #  # clear the Datashield R sessions and logout
#' #  datashield.logout(opals)
#' 
#' }
#'
ds.mean.o <- function(x=NULL, type='split', checks=FALSE, save.mean.Nvalid=FALSE, datasources=NULL){

#####################################################################################
#MODULE 1: IDENTIFY DEFAULT OPALS                                                   #
  # if no opal login details are provided look for 'opal' objects in the environment#
  if(is.null(datasources)){                                                         #
    datasources <- findLoginObjects()                                               #
  }                                                                                 #
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
  if(!('integer' %in% varClass) & !('numeric' %in% varClass)){                                          #
    stop("The input object must be an integer or a numeric vector.", call.=FALSE)             #
  }                                                                                           #
}                                                                                             #
###############################################################################################

###################################################################################################
#MODULE 4: EXTEND "type" argument to include "both" and enable valid alisases                     #
if(type == 'combine' | type == 'combined' | type == 'combines' | type == 'c') type <- 'combine'   #
if(type == 'split' | type == 'splits' | type == 's') type <- 'split'                              #
if(type == 'both' | type == 'b' ) type <- 'both'                                                  #
if(type != 'combine' & type != 'split' & type != 'both')                                          #
  stop('Function argument "type" has to be either "both", "combine" or "split"', call.=FALSE)     #
                                                                                                  #
#MODIFY FUNCTION CODE TO DEAL WITH ALL THREE TYPES                                                #
###################################################################################################


  cally <- paste0("meanDS.o(", x, ")")
  ss.obj <- opal::datashield.aggregate(datasources, as.symbol(cally))

  Nstudies <- length(datasources)
  ss.mat <- matrix(as.numeric(matrix(unlist(ss.obj),nrow=Nstudies,byrow=TRUE)[,1:4]),nrow=Nstudies)
  dimnames(ss.mat) <- c(list(names(ss.obj),names(ss.obj[[1]])[1:4]))

  ValidityMessage.mat <- matrix(matrix(unlist(ss.obj),nrow=Nstudies,byrow=TRUE)[,5],nrow=Nstudies)
  dimnames(ValidityMessage.mat) <- c(list(names(ss.obj),names(ss.obj[[1]])[5]))

  ss.mat.combined <- t(matrix(ss.mat[1,]))

  ss.mat.combined[1,1] <- (t(matrix(ss.mat[,3]))%*%ss.mat[,1])/sum(ss.mat[,3])
  ss.mat.combined[1,2] <- sum(ss.mat[,2])
  ss.mat.combined[1,3] <- sum(ss.mat[,3])
  ss.mat.combined[1,4] <- sum(ss.mat[,4])

  dimnames(ss.mat.combined) <- c(list("studiesCombined"),list(names(ss.obj[[1]])[1:4]))

  # IF save.mean.Nvalid==TRUE - KEY STUDY SPECIFIC STATISTICS ON APPROPRIATE OPAL SERVERS WITH ASSIGN FUNCTION
  if(save.mean.Nvalid==TRUE){

    for(j in 1:Nstudies){
      selected.opal <- datasources[j]
      mean.study.specific <- ss.mat[j,1]
      Nvalid.study.specific <- ss.mat[j,3]
      # SAVE VALIDITY MESSAGE
      opal::datashield.assign(selected.opal, "mean.study.specific", as.symbol(mean.study.specific))
      opal::datashield.assign(selected.opal, "Nvalid.study.specific", as.symbol(Nvalid.study.specific))
    }

    # SAVE KEY GLOBAL STATISTICS ON ALL OPAL SERVERS WITH ASSIGN FUNCTION
    mean.all.studies <- ss.mat.combined[1,1]
    Nvalid.all.studies <- ss.mat.combined[1,3]
    opal::datashield.assign(datasources, "mean.all.studies", as.symbol(mean.all.studies))
    opal::datashield.assign(datasources, "Nvalid.all.studies", as.symbol(Nvalid.all.studies))

#############################################################################
# MODULE 5: CHECK DATA OBJECTS SUCCESSFULLY CREATED                         #
  key.names <- extract("mean.all.studies")                                  #
  key.varname <- key.names$elements                                         #
  key.obj2lookfor <- key.names$holders                                      #
                                                                            #
  if(is.na(key.obj2lookfor)){                                               #
    key.defined <- isDefined(datasources, key.varname)                      #
  }else{                                                                    #
    key.defined <- isDefined(datasources, key.obj2lookfor)                  #
  }                                                                         #
                                                                            #
#if(key.defined==TRUE){                                                      #
#print("Data object <mean.all.studies> created successfully in all sources") #
#}                                                                           #
#############################################################################
}

#PRIMARY FUNCTION OUTPUT SUMMARISE RESULTS FROM
#AGGREGATE FUNCTION AND RETURN TO CLIENT-SIDE
  if (type=='split'){
    return(list(Mean.by.Study=ss.mat,Nstudies=Nstudies,ValidityMessage=ValidityMessage.mat))
  }

  if (type=="combine") {
    return(list(Global.Mean=ss.mat.combined,Nstudies=Nstudies,ValidityMessage=ValidityMessage.mat))
  }

  if (type=="both") {
    return(list(Mean.by.Study=ss.mat,Global.Mean=ss.mat.combined,Nstudies=Nstudies,ValidityMessage=ValidityMessage.mat))
  }

}
#ds.mean.o
