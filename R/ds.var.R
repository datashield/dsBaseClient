#'
#' @title Computes server-side vector variance 
#' @description Computes the variance of a given server-side vector. 
#' @details This function is similar to the R function \code{var}.
#' 
#' The function can carry out 3 types of analysis depending on
#' the argument \code{type}:\cr
#' (1) If \code{type} is set to \code{'combine'}, \code{'combined'}, 
#' \code{'combines'} or \code{'c'}, a global variance is calculated.\cr
#' (2) If \code{type} is set to \code{'split'}, \code{'splits'} or \code{'s'},
#'  the variance is calculated separately for each study. \cr
#' (3) If \code{type} is set to \code{'both'} or \code{'b'}, 
#' both sets of outputs are produced.
#' 
#' Server function called: \code{varDS}
#' @param x a character specifying the name of a numerical vector.
#' @param type a character string that represents the type of analysis to carry out.
#' This can be set as \code{'combine'}, \code{'combined'}, \code{'combines'},
#' \code{'split'}, \code{'splits'}, \code{'s'},
#' \code{'both'} or \code{'b'}. 
#' For more information see \strong{Details}.
#' @param checks logical. If TRUE  optional checks of model
#' components will be undertaken. Default is FALSE to save time. 
#' It is suggested that checks
#' should only be undertaken once the function call has failed.
#' @param datasources  a list of \code{\link[DSI]{DSConnection-class}} 
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#' @return \code{ds.var} returns to the client-side a list including:\cr
#' 
#'  \code{Variance.by.Study}: estimated variance, \code{Nmissing}
#' (number of missing observations), \code{Nvalid} (number of valid observations) and
#' \code{Ntotal} (sum of missing and valid observations) 
#' separately for each study (if \code{type = split} or \code{type = both}).\cr
#' \code{Global.Variance}: estimated variance, \code{Nmissing}, \code{Nvalid} and \code{Ntotal} 
#' across all studies combined (if \code{type = combine} or \code{type = both}). \cr
#' \code{Nstudies}: number of studies being analysed. \cr
#' \code{ValidityMessage}: indicates if the analysis was possible. \cr
#' @author DataSHIELD Development Team
#' @export
#' @examples
#' \dontrun{
#'
#'  ## Version 6, for version 5 see the Wiki
#'   
#'   # connecting to the Opal servers
#' 
#'   require('DSI')
#'   require('DSOpal')
#'   require('dsBaseClient')
#'
#'   builder <- DSI::newDSLoginBuilder()
#'   builder$append(server = "study1", 
#'                  url = "http://192.168.56.100:8080/", 
#'                  user = "administrator", password = "datashield_test&", 
#'                  table = "CNSIM.CNSIM1", driver = "OpalDriver")
#'   builder$append(server = "study2", 
#'                  url = "http://192.168.56.100:8080/", 
#'                  user = "administrator", password = "datashield_test&", 
#'                  table = "CNSIM.CNSIM2", driver = "OpalDriver")
#'   builder$append(server = "study3",
#'                  url = "http://192.168.56.100:8080/", 
#'                  user = "administrator", password = "datashield_test&", 
#'                  table = "CNSIM.CNSIM3", driver = "OpalDriver")
#'   logindata <- builder$build()
#'   
#'   connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D") 
#'   
#'   #Calculate the variance of a vector in the server-side
#'   
#'   ds.var(x = "D$LAB_TSC",
#'           type = "split",
#'           checks = FALSE,
#'           datasources = connections)
#'              
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(connections)
#' }
#'
ds.var <- function(x=NULL, type='split', checks=FALSE, datasources=NULL){

  #################################################################################################################
  #MODULE 1: IDENTIFY DEFAULT CONNECTIONS                                                                         #
  # look for DS connections                                                                                       #
  if(is.null(datasources)){								                          #
    datasources <- datashield.connections_find()                                                                  #
  }                                                                                                               #
                                                                                                                  #
  # ensure datasources is a list of DSConnection-class							          #
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){   #
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)               #
  }                                                                                                               #
  #################################################################################################################

  if(is.null(x)){
    stop("Please provide the name of the input object!", call.=FALSE)
  }
  
  # check if the input object is defined in all the studies
  isDefined(datasources, x)

  # beginning of optional checks - the process stops and reports as soon as one check fails
  if(checks){

    # check if the input object is defined in all the studies
    isDefined(datasources, x)
    
    # call the internal function that checks the input object is suitable in all studies        #
    varClass <- checkClass(datasources, x)                                                      #
    # the input object must be a numeric or an integer vector                                   #
    if(!('integer' %in% varClass) & !('numeric' %in% varClass)){                                #
      stop("The input object must be an integer or a numeric vector.", call.=FALSE)             #
    }                                                                                           #
  }                                                                                             #
  ###############################################################################################

  ###################################################################################################
  #MODULE: EXTEND "type" argument to include "both" and enable valid alisases                       #
  if(type == 'combine' | type == 'combined' | type == 'combines' | type == 'c') type <- 'combine'   #
  if(type == 'split' | type == 'splits' | type == 's') type <- 'split'                              #
  if(type == 'both' | type == 'b' ) type <- 'both'                                                  #
  #
  #MODIFY FUNCTION CODE TO DEAL WITH ALL THREE TYPES                                                #
  ###################################################################################################

  cally <- paste0("varDS(", x, ")")
  ss.obj <- DSI::datashield.aggregate(datasources, as.symbol(cally))

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
#ds.var
