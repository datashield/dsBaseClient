#'
#' @title Calculates the correlation of R objects in the server-side 
#' @description This function calculates the correlation of two variables or the correlation
#' matrix for the variables of an input data frame.
#' @details In addition to computing correlations; this function produces a table outlining the
#' number of complete cases and a table outlining the number of missing values to allow the
#' user to decide the 'relevance' of the correlation based on the number of complete
#' cases included in the correlation calculations.
#' 
#' If the argument \code{y} is not NULL, the dimensions of the object have to be 
#' compatible with the argument \code{x}. 
#' 
#' The function calculates the pairwise correlations based on casewise complete cases which means that
#' it omits all the rows in the input data frame that include at least one cell with a missing value,
#' before the calculation of correlations.
#' 
#' If \code{type} is set to \code{'split'} (default), the correlation of two variables or the
#' variance-correlation matrix of an input data frame and the number of complete cases and missing
#' values are returned for every single study. If type is set to \code{'combine'}, the pooled
#' correlation, the total number of complete cases and the total number of missing values aggregated
#' from all the involved studies, are returned.
#'  
#' Server function called: \code{corDS}
#' 
#' @param x a character string providing the name of the input vector, data frame or matrix.
#' @param y a character string providing the name of the input vector, data frame or matrix.
#' Default NULL. 
#' @param type a character string that represents the type of analysis to carry out. 
#' This must be set to \code{'split'} or \code{'combine'}.  Default \code{'split'}. For more information see details.
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} objects obtained after login. 
#' If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#' @return \code{ds.cor} returns a list containing the number of missing values in each variable,
#' the number of missing variables casewise, the correlation matrix, 
#' the number of used complete cases. The function applies two disclosure controls. The first disclosure
#' control checks that the number of variables is not bigger than a percentage of the individual-level records (the allowed
#' percentage is pre-specified by the 'nfilter.glm'). The second disclosure control checks that none of them is dichotomous
#' with a level having fewer counts than the pre-specified 'nfilter.tab' threshold.
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
#'   # Example 1: Get the correlation matrix of two continuous variables
#'   ds.cor(x="D$LAB_TSC", y="D$LAB_TRIG", type="combine", datasources = connections)
#'   
#'   # Example 2: Get the correlation matrix of the variables in a dataframe
#'   ds.dataFrame(x=c("D$LAB_TSC", "D$LAB_TRIG", "D$LAB_HDL", "D$PM_BMI_CONTINUOUS"), 
#'                newobj="D.new", check.names=FALSE, datasources=connections)
#'   ds.cor("D.new", type="combine", datasources = connections)
#' 
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(connections)
#'
#' }
#' @export
#' 
ds.cor <- function(x=NULL, y=NULL, type="split", datasources=NULL){

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
    warning("x is a matrix or a dataframe; y will be ignored and a correlation matrix computed for x!")
  }

  # name of the studies to be used in the output
  stdnames <- names(datasources)

  # call the server side function
  if(('matrix' %in% typ) | ('data.frame' %in% typ)){
    calltext <- call("corDS", x, NULL)
  }else{
    if(!(is.null(y))){
      calltext <- call("corDS", x, y)
    }else{
      calltext <- call("corDS", x, NULL)
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
          covariance[[i]][m,n] <- (1/(output[[i]][[3]][m,n]-1))*(output[[i]][[1]][m,n])-(1/(output[[i]][[3]][m,n]*(output[[i]][[3]][m,n]-1)))*output[[i]][[2]][m,n]*output[[i]][[2]][n,m]
        }
      }
      correlation[[i]] <- stats::cov2cor(covariance[[i]])
      results[[i]] <- list(output[[i]][[4]][[1]], output[[i]][[4]][[2]], correlation[[i]], output[[i]][[3]])
      n1 <- "Number of missing values in each variable"
      n2 <- "Number of missing values casewise"
      n3 <- "Correlation Matrix"
      n4 <- "Number of complete cases used"
      names(results[[i]]) <- c(n1, n2, n3, n4)
    }
    names(results) <- stdnames
  }
  else{
    if (type=="combine"){
      combined.sums.of.products <- matrix(0, ncol=dim(output[[1]][[1]])[2], nrow=dim(output[[1]][[1]])[1])
      combined.sums <- matrix(0, ncol=dim(output[[1]][[2]])[2], nrow=dim(output[[1]][[2]])[1])
      combined.complete.cases <- matrix(0, ncol=dim(output[[1]][[3]])[2], nrow=dim(output[[1]][[3]])[1])
      combined.missing.cases.vector <- matrix(0, ncol=dim(output[[1]][[4]][[1]])[2], nrow=dim(output[[1]][[4]][[1]])[1])
      combined.missing.cases.matrix <- matrix(0, ncol=dim(output[[1]][[4]][[2]])[2], nrow=dim(output[[1]][[4]][[2]])[1])
      combined.sums.of.squares <- matrix(0, ncol=dim(output[[1]][[5]])[2], nrow=dim(output[[1]][[5]])[1])
      for(i in 1:length(stdnames)){
        combined.sums.of.products <- combined.sums.of.products + output[[i]][[1]]
	      combined.sums <- combined.sums + output[[i]][[2]]
        combined.complete.cases <- combined.complete.cases + output[[i]][[3]]
        combined.missing.cases.vector <- combined.missing.cases.vector + output[[i]][[4]][[1]]
        combined.missing.cases.matrix <- combined.missing.cases.matrix + output[[i]][[4]][[2]]
        combined.sums.of.squares <- combined.sums.of.squares + output[[i]][[5]]
      }
      
  	combined.covariance <- matrix(0, ncol=dim(output[[1]][[1]])[2], nrow=dim(output[[1]][[1]])[1])
	  colnames(combined.covariance) <- colnames(output[[1]][[1]])
    rownames(combined.covariance) <- colnames(output[[1]][[1]])
    for(m in 1:dim(output[[i]][[1]])[1]){
      for(n in 1:dim(output[[i]][[1]])[1]){
        combined.covariance[m,n] <- (1/(combined.complete.cases[m,n]-1))*(combined.sums.of.products[m,n])-(1/(combined.complete.cases[m,n]*(combined.complete.cases[m,n]-1)))*combined.sums[m,n]*combined.sums[n,m]
	    }
    }
    
    combined.correlation <- stats::cov2cor(combined.covariance)
    
	  results <- list(combined.missing.cases.vector, combined.missing.cases.matrix, combined.complete.cases, combined.correlation)
      n1 <- "Number of missing values in each variable"
      n2 <- "Number of missing values casewise"
	    n3 <- "Number of complete cases used"
	    n4 <- "Correlation Matrix"
	    names(results) <- c(n1, n2, n3, n4)
    }else{
      stop('Function argument "type" has to be either "combine" or "split"', call.=FALSE)
    }
  }

  return(results)

}
