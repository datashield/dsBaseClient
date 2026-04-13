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
#'           datasources = connections)
#'              
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(connections)
#' }
#'
ds.var <- function(x=NULL, type='split', classConsistencyCheck=FALSE, datasources=NULL){

  datasources <- .set_datasources(datasources)

  if(is.null(x)){
    stop("Please provide the name of the input object!", call.=FALSE)
  }

  ###################################################################################################
  #MODULE: EXTEND "type" argument to include "both" and enable valid alisases                       #
  if(type == 'combine' | type == 'combined' | type == 'combines' | type == 'c') type <- 'combine'   #
  if(type == 'split' | type == 'splits' | type == 's') type <- 'split'                              #
  if(type == 'both' | type == 'b' ) type <- 'both'                                                  #
  #
  #MODIFY FUNCTION CODE TO DEAL WITH ALL THREE TYPES                                                #
  ###################################################################################################

  cally <- call("varDS", x)
  ss.obj <- DSI::datashield.aggregate(datasources, cally)

  if(classConsistencyCheck){
    .checkClassConsistency(ss.obj)
  }

  Nstudies <- length(datasources)
  EstimatedVar <- c()
  Nvalid <- c()
  Nmissing <- c()
  Ntotal <- c()
  for (i in 1:Nstudies){
    EstimatedVar[i] <- ss.obj[[i]]$SumOfSquares/(ss.obj[[i]]$Nvalid-1) - (ss.obj[[i]]$Sum)^2/(ss.obj[[i]]$Nvalid*(ss.obj[[i]]$Nvalid-1))
    Nvalid[i] <- ss.obj[[i]]$Nvalid
    Nmissing[i] <- ss.obj[[i]]$Nmissing
    Ntotal[i] <- ss.obj[[i]]$Ntotal
  }
  ss.mat <- matrix(c(EstimatedVar,Nmissing,Nvalid,Ntotal),nrow=Nstudies)
  dimnames(ss.mat) <- c(list(names(ss.obj),c('EstimatedVar','Nmissing','Nvalid','Ntotal')))

  ss.mat.combined <- t(matrix(ss.mat[1,]))

  GlobalSum.new <- 0
  GlobalSumSquares.new <- 0
  GlobalNvalid.new <- 0
  for (i in 1:Nstudies){
    GlobalSum <- GlobalSum.new +  ss.obj[[i]]$Sum
    GlobalSumSquares <- GlobalSumSquares.new +  ss.obj[[i]]$SumOfSquares
    GlobalNvalid <- GlobalNvalid.new +  ss.obj[[i]]$Nvalid
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
    return(list(Variance.by.Study=ss.mat,Nstudies=Nstudies))
  }

  if (type=="combine"){
    return(list(Global.Variance=ss.mat.combined,Nstudies=Nstudies))
  }

  if (type=="both"){
    return(list(Variance.by.Study=ss.mat,Global.Variance=ss.mat.combined,Nstudies=Nstudies))
  }

}
#ds.var
