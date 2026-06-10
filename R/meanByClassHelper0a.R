#'
#' @title Computes the mean values of a numeric vector across a factor vector
#' @description This is an internal function.
#' @details This function is called by the function 'ds.meanByClass' to produce the final tables
#' if the user specifies two loose vectors.
#' @param a a character, the name of a numeric vector.
#' @param b a character, the name of a factor vector.
#' @param type a character which represents the type of analysis to carry out. If \code{type} is set to
#' 'combine', a pooled table of results is generated. If \code{type} is set to 'split', a table of results
#' is generated for each study.
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} objects obtained after login. If the <datasources>
#' the default set of connections will be used: see \link[DSI]{datashield.connections_default}.
#' @return a table or a list of tables that hold the length of the numeric variable and its mean
#' and standard deviation in each subgroup (subset).
#' @keywords internal
#' @author Gaye, A.
#'
meanByClassHelper0a <- function(a, b, type, datasources){

  # generate a data frame from the two vector
  x <- c(a,b)
  aa <- unlist(strsplit(a, split='$', fixed=TRUE))
  bb <- unlist(strsplit(b, split='$', fixed=TRUE))
  if(length(aa) > 1){
    v1 <- aa[2]
  }else{
    v1 <- aa[1]
  }
  if(length(bb) > 1){
    v2 <- bb[2]
  }else{
    v2 <- bb[1]
  }
  v <- c(v1, v2)
  newD <- "X"

  x.names.transmit   <- paste(x, collapse=",")
  col.names.transmit <- paste(v, collapse=',')
  cally <- call("dataFrameDS", x.names.transmit, NULL, FALSE, TRUE, col.names.transmit, TRUE, FALSE)
  DSI::datashield.assign(datasources, newD, cally)

  # get the 'loose' names of the variables and call the function that generate the results

  output <- meanByClassHelper0b(newD, v1, v2, type, datasources)
  return(output)
}
