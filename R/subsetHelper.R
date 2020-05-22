#'
#' @title Compares subset and original object sizes and eventually carries out subsetting
#' @description This is an internal function.
#' @details This function is called by the function 'ds.subset' to ensure that the requested subset
#' is not larger than the original object.
#' @param datasources  a list of \code{\link{DSConnection-class}} 
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link{datashield.connections_default}}.
#' @param data a string character, the name of the dataframe or the factor vector and the range of the subset.
#' @param rs a vector of two integers, the indices of the rows de extract.
#' @param cs a vector of two integers or one or more characters.
#' @keywords internal
#' @return a message or the class of the object if the object has the same class in all studies.
#'
subsetHelper <- function(dts, data, rs=NULL, cs=NULL){

  # if the size of the requested subset is greater than that of original inform the user and stop the process
  dims <- DSI::datashield.aggregate(dts, call("dimDS", data))
  fail <- c(0,0)

  if(!(is.null(rs))){
    if(length(rs) > dims[[1]][1] ){
      fail[1] <- 1
    }
  }

  if(!(is.null(cs))){
    if(length(cs) > dims[[1]][2]){
      fail[2] <- 1
    }
  }
  return(fail)

}
