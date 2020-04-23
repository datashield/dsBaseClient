#'
#' @title Calculates the correlation of R objects in the server-side 
#' @description This function calculates the correlation of two variables or the correlation
#' matrix for the variables of an input data frame.
#' @details In addition to computing correlations; this function produces a table outlining the
#' number of complete cases and a table outlining the number of missing values to allow for the
#' user to decide the 'relevance' of the correlation based on the number of complete
#' cases included in the correlation calculations.
#' 
#' If the argument \code{y} is not NULL, the dimensions of the object have to be 
#' compatible with the argument \code{x}. 
#' 
#' If \code{naAction} is set to \code{'casewise.complete'}, then the function omits all the rows
#' in the whole data frame that include at least one cell with a missing value before the calculation of correlations.
#' If \code{naAction} is set to \code{'pairwise.complete'} (default),
#'  then the function divides the input data frame to 
#' subset data frames formed by each pair between two variables 
#' (all combinations are considered) and omits the rows
#' with missing values at each pair separately and then calculates the correlations of those pairs.
#' 
#'  If \code{type} is set to \code{'split'} (default), the correlation of two variables or the
#'  variance-correlation matrix of an input data frame and the number of 
#'  complete cases and missing values are returned for every single study. 
#'  If type is set to \code{'combine'}, the pooled correlation, the total number of complete cases 
#'  and the total number of missing values aggregated from all the involved studies, are returned.
#'  
#'  Server function called: \code{corDS}
#' 
#' @param x a character string providing the name of the input vector, data frame or matrix.
#' @param y a character string providing the name of the input vector, data frame or matrix.
#' Default NULL. 
#' @param naAction a character string giving a method for computing correlations in the
#' presence of missing values. This must be set to  \code{'casewise.complete'} or
#' \code{'pairwise.complete'}. Default \code{'pairwise.complete'}. For more information see details. 
#' @param type a character string that represents the type of analysis to carry out. 
#' This must be set to \code{'split'} or \code{'combine'}.  Default \code{'split'}. For more information see details.
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login. 
#' If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link{datashield.connections_default}}.
#' @return \code{ds.cor} returns a list containing the number of missing values in each variable,
#' the number of missing variables casewise or
#' pairwise depending on the argument \code{naAction}, the correlation matrix, the number of used complete cases
#' and an error message which indicates whether or not the input variables pass the disclosure control (i.e. none of them
#' is dichotomous with a level having fewer counts than the pre-specified threshold). If any of the input variables do not
#' pass the disclosure control then all the output values are replaced with NAs. If all the variables are valid and pass
#' the control, then the output matrices are returned and also an error message is returned but it is replaced by NA.
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
#'   # Calculate the correlation between two vectors
#'   ds.assign(newobj='labhdl', toAssign='D$LAB_HDL')
#'   ds.assign(newobj='labtsc', toAssign='D$LAB_TSC')
#'   ds.assign(newobj='gender', toAssign='D$GENDER')
#'   ds.cor(x = 'labhdl',
#'          y = 'labtsc',
#'          naAction = 'pairwise.complete',
#'          type = 'combine',
#'          datasources = connections)
#'   ds.cor(x = 'labhdl',
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
ds.cor <- function(x=NULL, y=NULL, naAction='pairwise.complete', type="split", datasources=NULL){

  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  if(is.null(x)){
    stop("x=NULL. Please provide the name of a matrix or dataframe or the names of two numeric vectors!", call.=FALSE)
  }else{
    defined <- isDefined(datasources, x)
  }

  # check the type of the input objects
  typ <- checkClass(datasources, x)
  
  if(('numeric' %in% typ) | ('integer' %in% typ) | ('factor' %in% typ)){
    if(is.null(y)){
      stop("If x is a numeric vector, y must be a numeric vector!", call.=FALSE)
    }else{
      defined2 <- isDefined(datasources, y)
      typ2 <- checkClass(datasources, y)
    }
  }
  
  if(('matrix' %in% typ) | ('data.frame' %in% typ) & !(is.null(y))){
    y <- NULL
    warning("x is a matrix or a dataframe; y will be ignored and a correlation matrix computed for x!")
  }

  # name of the studies to be used in the output
  stdnames <- names(datasources)

  # call the server side function
  if(('matrix' %in% typ) | ('data.frame' %in% typ)){
    calltext <- call("corDS", x, NULL, naAction)
  }else{
    if(!(is.null(y))){
      calltext <- call("corDS", x, y, naAction)
    }else{
      calltext <- call("corDS", x, NULL, naAction)
    }
  }
  output <- DSI::datashield.aggregate(datasources, calltext)

  if (type=="split"){
    covariance <- list()
    sqrt.diag <- list()
    correlation <- list()
    results <- list()
    for(i in 1:length(stdnames)){
      covariance[[i]] <- matrix(0, ncol=dim(output[[i]][[1]])[2], nrow=dim(output[[i]][[1]])[1])
      correlation[[i]] <- matrix(0, ncol=dim(output[[i]][[1]])[2], nrow=dim(output[[i]][[1]])[1])
      colnames(correlation[[i]]) <- colnames(output[[i]][[1]])
      rownames(correlation[[i]]) <- colnames(output[[i]][[1]])
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
      if (naAction=='casewise.complete'){
	    sqrt.diag[[i]] <- sqrt(1/diag(covariance[[i]]))
        correlation[[i]] <- rep(sqrt.diag[[i]], dim(covariance[[i]])[1]) * covariance[[i]] * rep(sqrt.diag[[i]], each = dim(covariance[[i]])[1])
	  }
	  if (naAction=='pairwise.complete'){
        sqrt.diag[[i]] <- sqrt(1/(output[[i]][[6]]))
        correlation[[i]] <- sqrt.diag[[i]] * covariance[[i]] * t(sqrt.diag[[i]])
	  }

	  results[[i]] <- list(output[[i]][[4]][[1]], output[[i]][[4]][[2]], correlation[[i]], output[[i]][[3]], output[[i]][[5]])
      n1 <- "Number of missing values in each variable"
      if(naAction=='casewise.complete'){
        n2 <- "Number of missing values casewise"
      }
      if(naAction=='pairwise.complete'){
        n2 <- "Number of missing values pairwise"
      }
      n3 <- "Correlation Matrix"
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
      combined.sums.of.squares <- matrix(0, ncol=dim(output[[1]][[7]])[2], nrow=dim(output[[1]][[7]])[1])
      for(i in 1:length(stdnames)){
        combined.sums.of.products <- combined.sums.of.products + output[[i]][[1]]
	    combined.sums <- combined.sums + output[[i]][[2]]
        combined.complete.cases <- combined.complete.cases + output[[i]][[3]]
        combined.missing.cases.vector <- combined.missing.cases.vector + output[[i]][[4]][[1]]
        combined.missing.cases.matrix <- combined.missing.cases.matrix + output[[i]][[4]][[2]]
	    combined.error.message[[i]] <- output[[i]][[5]]
        combined.sums.of.squares <- combined.sums.of.squares + output[[i]][[7]]
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

    if (naAction=='casewise.complete'){
	  combined.sqrt.diag <- sqrt(1/diag(combined.covariance))
      combined.correlation <- rep(combined.sqrt.diag, dim(combined.covariance)[1]) * combined.covariance * rep(combined.sqrt.diag, each = dim(combined.covariance)[1])
    }
	if (naAction=='pairwise.complete'){
	  combined.variance <- matrix(0, ncol=dim(output[[1]][[6]])[2], nrow=dim(output[[1]][[6]])[1])
      for(i in 1:length(stdnames)){
       combined.variance <- combined.variance + (output[[i]][[3]]-matrix(1, ncol=dim(output[[i]][[3]])[2], nrow=dim(output[[i]][[3]])[1])) * output[[i]][[6]]
      }
      combined.variance <- combined.variance / (combined.complete.cases-matrix(length(stdnames), ncol=dim(combined.complete.cases)[2], nrow=dim(combined.complete.cases)[1]))

	  combined.sqrt.diag <- sqrt(1/(combined.variance))
      combined.correlation <- combined.sqrt.diag * combined.covariance * t(combined.sqrt.diag)
    }
	  results <- list(combined.missing.cases.vector, combined.missing.cases.matrix, combined.correlation, combined.complete.cases, combined.error.message)
      n1 <- "Number of missing values in each variable"
      if(naAction=='casewise.complete'){
        n2 <- "Number of missing values casewise"
      }
      if(naAction=='pairwise.complete'){
        n2 <- "Number of missing values pairwise"
      }
      n3 <- "Correlation Matrix"
	  n4 <- "Number of complete cases used"
      n5 <- "Error message"
      names(results) <- c(n1, n2, n3, n4, n5)
    }else{
      stop('Function argument "type" has to be either "combine" or "split"')
    }
  }

  return(results)

}
