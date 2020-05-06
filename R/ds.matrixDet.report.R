#' @title ds.matrixDet.report calling aggregate function matrixDetDS1
#' @description Calculates the determinant of a square matrix A and returns
#' the result to the clientside
#' @details Calculates the determinant of a square matrix (for additional
#' information see help for {determinant} function in native R). This operation is only
#' possible if the number of columns and rows of A are the same.
#' @param M1  A character string specifying the name of the matrix for which
#' determinant to be calculated
#' @param logarithm logical. Default is FALSE, which returns the
#' determinant itself, TRUE returns the logarithm of the modulus of the determinant.
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login. If the <datasources>
#' the default set of connections will be used: see \link{datashield.connections_default}.
#' @return the matrix determinant for A to the clientside. Calculated separately
#' for each study. The determinant is reported as a two component list. Element 1
#' is $modulus and element 2 is $sign. If logarithm=FALSE: $modulus reports the
#' absolute value of the determinant and is therefore always positive. $sign
#' indicates whether the determinant is positive ($sign=1) or negative ($sign=-1).
#' $modulus has an attribute [attr(,"logarithm")] which is FALSE if the argument
#' <logarithm> was FALSE - this enables you to look at results post-hoc to determine
#' whether the logarithm argument was TRUE or FALSE. If you wish to generate the
#' actual determinant if logarithm=FALSE it is easiest to calculate $modulus*$sign.
#' If logarithm=TRUE: $modulus reports the log (to base e) of the absolute value
#' of the determinant. $sign again reports whether the determinant is positive
#' ($sign=1) or negative ($sign=-1). The attribute of $modulus [attr(,"logarithm")]
#' is now TRUE. If you wish to generate the actual determinant when logarithm=TRUE
#' you calculate exp($modulus)*$sign. If the function fails in any study for
#' a reason which is identified, an explanatory error message is returned
#' instead of the object containing the calculated matrix determinant
#' @author Paul Burton for DataSHIELD Development Team
#' @export
#'
ds.matrixDet.report<-function(M1=NULL, logarithm=FALSE, datasources=NULL){

  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  # check if user has provided the name of matrix representing M1
  if(is.null(M1)){
    return("Error: Please provide the name of the matrix representing M1")
  }

  # if no value or invalid value specified for logarithm, then specify a default
  if(is.null(logarithm))
  {
  logarithm<-FALSE
  }

  if(logarithm!=TRUE)
  {
  logarithm<-FALSE
  }

  # CALL THE MAIN SERVER SIDE AGGREGATE FUNCTION
  calltext <- call("matrixDetDS1", M1, logarithm)
  output<-DSI::datashield.aggregate(datasources, calltext)

  return(list(matrix.determinant=output))
}
#ds.matrixDet.report
