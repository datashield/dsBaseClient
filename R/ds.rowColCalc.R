#'
#' @title Computes rows and columns sums and means in the server-side
#' @description Computes sums and means of rows or columns 
#' of a numeric matrix or data frame on the server-side.
#' @details The function is similar to R base functions \code{rowSums}, \code{colSums},
#' \code{rowMeans} and \code{colMeans} with some restrictions.
#' 
#' The results of the calculation are not returned to the user if they are potentially
#' revealing i.e. if the number of rows is less than the allowed number of observations.
#' 
#' Server functions called: \code{classDS}, \code{dimDS} and \code{colnamesDS} 
#' @param x a character string specifying  the name of a matrix or a data frame.
#' @param operation a character string that indicates the operation to carry out:
#' \code{"rowSums"}, \code{"colSums"}, \code{"rowMeans"} or \code{"colMeans"}.
#' @param newobj a character string that provides the name for the output variable 
#' that is stored on the data servers. Default \code{rowcolcalc.newobj}.
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} objects obtained after login. 
#' If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#' @return \code{ds.rowColCalc} returns to the server-side  rows and columns sums and means. 
#' @author DataSHIELD Development Team
#' @examples
#' \dontrun{
#' 
#'   ## Version 6, for version 5 see the Wiki 
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
#'   myvar <- list("LAB_TSC","LAB_HDL")
#'    
#'   # Log onto the remote Opal training servers
#'   connections <- DSI::datashield.login(logins = logindata, assign = TRUE, 
#'   variables = myvar, symbol = "D") 
#'
#'   
#'   #Calculate the colSums
#'   
#'   ds.rowColCalc(x = "D",
#'                 operation = "colSums", 
#'                 newobj = "D.rowSums", 
#'                 datasources = connections)
#'                 
#'   #Clear the Datashield R sessions and logout
#'   datashield.logout(connections) 
#'
#' }
#' @export
#' 
ds.rowColCalc <- function(x=NULL, operation=NULL, newobj=NULL, datasources=NULL){

  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }

  if(is.null(x)){
    stop("Please provide the name of a data.frame or matrix!", call.=FALSE)
  }

  # check if the input object(s) is(are) defined in all the studies
  defined <- isDefined(datasources, x)

  # call the internal function that checks the input object is of the same class in all studies.
  typ <- checkClass(datasources, x)

  # if the input object is not a matrix or a dataframe stop
  if(!('data.frame' %in% typ) & !('matrix' %in% typ)){
    stop("The input vector must be of type 'data.frame' or a 'matrix'!", call.=FALSE)
  }

  # number of studies and their names
  numsources <- length(datasources)
  stdnames <- names(datasources)

  # we want to deal only with two dimensional tables
  dim2 <- c()
  for(i in 1:numsources){
    dims <- DSI::datashield.aggregate(datasources[i], call("dimDS", x))
    if(length(dims[[1]]) != 2){
      stop("The input table in ", stdnames[i]," has more than two dimensions. Only strutures of two dimensions are allowed", call.=FALSE)
    }
    dim2 <- append(dim2, dims[[1]][2])
  }

  # check that, for each study,  all the columns of the input table are of 'numeric' type
  dtname <- x
  for(i in 1:numsources){
    cols <- DSI::datashield.aggregate(datasources[i], call("colnamesDS", x))
    for(j in 1:dim2[i]){
      cally <- call("classDS", paste0(dtname, "$", cols[[1]][j]))
      res <- DSI::datashield.aggregate(datasources[i], cally)
      if(res[[1]] != 'numeric' & res[[1]] != 'integer'){
        stop("One or more columns of ", dtname, " are not of numeric type, in ",  stdnames[i], ".", call.=FALSE)
      }
    }
  }

  ops <- c("rowSums","colSums","rowMeans","colMeans")
  if(is.null(operation)){
    message(" ALERT!")
    message(" Please indicate the calculation required.")
    stop("'operation' = NULL. Please set it to 'rowSums', 'colSums', 'rowMeans' or 'colMeans'", call.=FALSE)
  }else{
    if(!any(operation %in% ops)){
      stop("'operation' must be set to: 'rowSums', 'colSums', 'rowMeans' or 'colMeans'")
    }
  }

  # operation to carry out
  indx <- which(ops == operation)

  # create a name by default if user did not provide a name for the new variable
  if(is.null(newobj)){
    newobj <- "rowcolcalc.newobj"
  }

  # call the server side function that does the job
  cally <-  paste0("rowColCalcDS(", x, ",", indx, ")")
  DSI::datashield.assign(datasources, newobj, as.symbol(cally))

  # check that the new object has been created and display a message accordingly
  finalcheck <- isAssigned(datasources, newobj)

}
