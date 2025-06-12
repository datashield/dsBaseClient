#'
#' @title Calculates the covariance of R objects in the server-side
#' @description This function calculates the covariance of two variables or the variance-covariance
#' matrix for the variables of an input data frame.
#' @details In addition to computing covariances; this function produces a table outlining the
#' number of complete cases and a table outlining the number of missing values to allow for the
#' user to decide about the 'relevance' of the covariance based on the number of complete
#' cases included in the covariance calculations. 
#' 
#' If the argument \code{y} is not NULL, the dimensions of the object have to be 
#' compatible with the argument \code{x}. 
#' 
#' If \code{naAction} is set to \code{'casewise.complete'}, then the function omits all the rows
#' in the whole data frame that include at least one cell with a missing value before the calculation of covariances.
#' If \code{naAction} is set to \code{'pairwise.complete'} (default),
#' then the function divides the input data frame to 
#' subset data frames formed by each pair between two variables 
#' (all combinations are considered) and omits the rows
#' with missing values at each pair separately and then calculates the covariances of those pairs.
#' 
#' If \code{type} is set to \code{'split'} (default), the covariance of two variables or the
#' variance-covariance matrix of an input data frame and the number of 
#' complete cases and missing values are returned for every single study. 
#' If type is set to \code{'combine'}, the pooled covariance, the total number of complete cases 
#' and the total number of missing values aggregated from all the involved studies, are returned.
#'  
#'  Server function called: \code{covDS}
#' 
#' 
#' @param x a character string providing the name of the input vector, data frame or matrix.
#' @param y a character string providing the name of the input vector, 
#' data frame or matrix. Default NULL.
#' @param naAction a character string giving a method for computing covariances in the
#' presence of missing values. This must be set to  \code{'casewise.complete'} or
#' \code{'pairwise.complete'}. Default \code{'pairwise.complete'}. For more information see details.
#' @param type a character string that represents the type of analysis to carry out. 
#' This must be set to \code{'split'} or \code{'combine'}.  Default \code{'split'}. For more information see details.
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} objects obtained after login. 
#' If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#' @return \code{ds.cov} returns a list containing the number of missing values in each variable, the number of missing values
#' casewise or pairwise depending on the argument \code{naAction}, the covariance matrix, the number of used complete cases
#' and an error message which indicates whether or not the input variables pass the disclosure controls. The first disclosure
#' control checks that the number of variables is not bigger than a percentage of the individual-level records (the allowed
#' percentage is pre-specified by the 'nfilter.glm'). The second disclosure control checks that none of them is dichotomous
#' with a level having fewer counts than the pre-specified 'nfilter.tab' threshold. If any of the input variables do not pass 
#' the disclosure controls then all the output values are replaced with NAs. If all the variables are valid and pass
#' the controls, then the output matrices are returned and also an error message is returned but it is replaced by NA.
#' @author DataSHIELD Development Team
#' @examples
#' \dontrun{
#'
#' ## Version 6, for version 5 see the Wiki
#'   # Connecting to the Opal servers
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
#'   # Log onto the remote Opal training servers
#'   connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D") 
#'   
#'   # Calculate the covariance between two vectors
#'   ds.assign(newobj='labhdl', toAssign='D$LAB_HDL', datasources = connections)
#'   ds.assign(newobj='labtsc', toAssign='D$LAB_TSC', datasources = connections)
#'   ds.assign(newobj='gender', toAssign='D$GENDER', datasources = connections)
#'   ds.cov(x = 'labhdl',
#'          y = 'labtsc',
#'          naAction = 'pairwise.complete',
#'          type = 'combine',
#'          datasources = connections)
#'   ds.cov(x = 'labhdl',
#'          y = 'gender',
#'          naAction = 'pairwise.complete',
#'          type = 'combine',
#'          datasources = connections[1]) #only the first Opal server is used ("study1")
#' 
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(connections)
#'
#' }
#' @export
#'
ds.cov <- function(x=NULL, y=NULL, naAction='pairwise.complete', type="split", datasources=NULL){

  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }

  if(is.null(x)){
    stop("x=NULL. Please provide the name of a matrix or dataframe or the names of two numeric vectors!", call.=FALSE)
  }else{
    isDefined(datasources, x)
  }

  # check the type of the input objects
  typ <- checkClass(datasources, x)
  
  if(('numeric' %in% typ) | ('integer' %in% typ) | ('factor' %in% typ)){
    if(is.null(y)){
      stop("If x is a numeric vector, y must be a numeric vector!", call.=FALSE)
    }else{
      isDefined(datasources, y)
      typ2 <- checkClass(datasources, y)
    }
  }
  
  if(('matrix' %in% typ) | ('data.frame' %in% typ) & !(is.null(y))){
    y <- NULL
    warning("x is a matrix or a dataframe; y will be ignored and a covariance matrix computed for x!")
  }

  # name of the studies to be used in the output
  stdnames <- names(datasources)

  # call the server side function
  if(('matrix' %in% typ) | ('data.frame' %in% typ)){
    calltext <- call("covDS", x, NULL, naAction)
  }else{
    if(!(is.null(y))){
      calltext <- call("covDS", x, y, naAction)
    }else{
      calltext <- call("covDS", x, NULL, naAction)
    }
  }
  output <- DSI::datashield.aggregate(datasources, calltext)
  
  if (type=="split"){
    covariance <- list()
    results <- list()
    for(i in 1:length(stdnames)){
      covariance[[i]] <- matrix(0, ncol=dim(output[[i]][[1]])[2], nrow=dim(output[[i]][[1]])[1])
      colnames(covariance[[i]]) <- colnames(output[[i]][[1]])
      rownames(covariance[[i]]) <- colnames(output[[i]][[1]])
      for(m in 1:dim(output[[i]][[1]])[1]){
        for(n in 1:dim(output[[i]][[1]])[2]){
          if (naAction=='pairwise.complete'){
            covariance[[i]][m,n] <- (1/(output[[i]][[3]][m,n]-1))*(output[[i]][[1]][m,n])-(1/(output[[i]][[3]][m,n]*(output[[i]][[3]][m,n]-1)))*output[[i]][[2]][m,n]*output[[i]][[2]][n,m]
          }
          if (naAction=='casewise.complete'){
            covariance[[i]][m,n] <- (1/(output[[i]][[3]][m,n]-1))*(output[[i]][[1]][m,n])-(1/(output[[i]][[3]][m,n]*(output[[i]][[3]][m,n]-1)))*output[[i]][[2]][m]*output[[i]][[2]][n]
          }
       }
      }
      results[[i]] <- list(output[[i]][[4]][[1]], output[[i]][[4]][[2]], covariance[[i]], output[[i]][[3]], output[[i]][[5]])
      n1 <- "Number of missing values in each variable"
      if(naAction=='casewise.complete'){
        n2 <- "Number of missing values casewise"
      }
      if(naAction=='pairwise.complete'){
        n2 <- "Number of missing values pairwise"
      }
      n3 <- "Variance-Covariance Matrix"
	  n4 <- "Number of complete cases used"
      n5 <- "Error message"
      names(results[[i]]) <- c(n1, n2, n3, n4, n5)
    }
  }else{
    if (type=="combine"){
      combined.sums.of.products <- matrix(0, ncol=dim(output[[1]][[1]])[2], nrow=dim(output[[1]][[1]])[1])
      combined.sums <- matrix(0, ncol=dim(output[[1]][[2]])[2], nrow=dim(output[[1]][[2]])[1])
      combined.complete.cases <- matrix(0, ncol=dim(output[[1]][[3]])[2], nrow=dim(output[[1]][[3]])[1])
      combined.missing.cases.vector <- matrix(0, ncol=dim(output[[1]][[4]][[1]])[2], nrow=dim(output[[1]][[4]][[1]])[1])
      combined.missing.cases.matrix <- matrix(0, ncol=dim(output[[1]][[4]][[2]])[2], nrow=dim(output[[1]][[4]][[2]])[1])
      combined.error.message <- list()
      for(i in 1:length(stdnames)){
        combined.sums.of.products <- combined.sums.of.products + output[[i]][[1]]
	      combined.sums <- combined.sums + output[[i]][[2]]
        combined.complete.cases <- combined.complete.cases + output[[i]][[3]]
        combined.missing.cases.vector <- combined.missing.cases.vector + output[[i]][[4]][[1]]
        combined.missing.cases.matrix <- combined.missing.cases.matrix + output[[i]][[4]][[2]]
	      combined.error.message[[i]] <- output[[i]][[5]]
      }

	combined.covariance <- matrix(0, ncol=dim(output[[1]][[1]])[2], nrow=dim(output[[1]][[1]])[1])
	colnames(combined.covariance) <- colnames(output[[1]][[1]])
      rownames(combined.covariance) <- colnames(output[[1]][[1]])
      for(m in 1:dim(output[[i]][[1]])[1]){
        for(n in 1:dim(output[[i]][[1]])[1]){
          if (naAction=='pairwise.complete'){
            combined.covariance[m,n] <- (1/(combined.complete.cases[m,n]-1))*(combined.sums.of.products[m,n])-(1/(combined.complete.cases[m,n]*(combined.complete.cases[m,n]-1)))*combined.sums[m,n]*combined.sums[n,m]
          }
          if (naAction=='casewise.complete'){
            combined.covariance[m,n] <- (1/(combined.complete.cases[m,n]-1))*(combined.sums.of.products[m,n])-(1/(combined.complete.cases[m,n]*(combined.complete.cases[m,n]-1)))*combined.sums[m]*combined.sums[n]
          }
        }
      }

      results <- list(combined.missing.cases.vector, combined.missing.cases.matrix, combined.covariance, combined.complete.cases, combined.error.message)
      n1 <- "Number of missing values in each variable"
      if(naAction=='casewise.complete'){
        n2 <- "Number of missing values casewise"
      }
      if(naAction=='pairwise.complete'){
        n2 <- "Number of missing values pairwise"
      }
      n3 <- "Variance-Covariance Matrix"
	  n4 <- "Number of complete cases used"
      n5 <- "Error message"
      names(results) <- c(n1, n2, n3, n4, n5)
    }else{
      stop('Function argument "type" has to be either "combine" or "split"')
    }
  }

  return(results)

}
