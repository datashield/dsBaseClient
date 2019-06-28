#' 
#' @title Computes sums and means of rows or columns of numeric matrix or data frame
#' @description The function is similar to R base functions 'rowSums', 'colSums',
#' 'rowMeans' and 'colMeans' with some restrictions.
#' @details The results of calculation are not returned to the user if they are potentially 
#' revealing i.e. if the number of rows is less than the allowed number of observations.
#' @param x a character, the name of a matrix or a dataframe
#' @param operation a character string which indicates the operation to carry out:
#' "rowSums", "colSums", "rowMeans" or "colMeans".
#' @param newobj the name of the new object. If this argument is set to NULL, the name of the new 
#' variable, set by default, is 'rowColCalc_out'.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @return nothing is returned to the client, the new object is stored on the server side.
#' @export
#' @author Gaye, A.
#' @examples
#' \dontrun{
#' 
#'   # load that contains the login details
#'   data(logindata)
#'
#'   # login and assign two variables
#'   myvar  <-  list("LAB_TSC","LAB_HDL")
#'   opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#'   # calculate the sum of each row of the above assigned dataset (default name 'D')
#'   ds.rowColCalc(x='D', operation='rowSums', newobj='rsum_D')
#' 
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(opals)
#' 
#' }
#' 
ds.rowColCalc = function(x=NULL, operation=NULL, newobj=NULL, datasources=NULL){
  
  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }
  
  if(is.null(x)){
    stop("Please provide the name of a data.frame or matrix!", call.=FALSE)
  }
  
  # check if the input object(s) is(are) defined in all the studies
  defined <- isDefined(datasources, x)
  
  # call the internal function that checks the input object is of the same class in all studies.
  typ <- checkClass(datasources, x)
  
  # if the input object is not a matrix or a dataframe stop
  if(typ != 'data.frame' & typ != 'matrix'){
    stop("The input vector must be of type 'data.frame' or a 'matrix'!", call.=FALSE)
  }
  
  # number of studies and their names
  numsources <- length(datasources)
  stdnames <- names(datasources)
  
  # we want to deal only with two dimensional tables
  dim2 <- c()
  for(i in 1:numsources){
    dims <- opal::datashield.aggregate(datasources[i], paste0("dim(", x, ")"))
    if(length(dims[[1]]) != 2){
      stop("The input table in ", stdnames[i]," has more than two dimensions. Only strutures of two dimensions are allowed", call.=FALSE)
    }
    dim2 <- append(dim2, dims[[1]][2])
  }
  
  # check that, for each study,  all the columns of the input table are of 'numeric' type
  dtname <- x
  for(i in 1:numsources){
    cols <- opal::datashield.aggregate(datasources[i], paste0("colnames(", x, ")"))
    for(j in 1:dim2[i]){
      cally <- paste0("class(", dtname, "$", cols[[1]][j], ")")
      res <- opal::datashield.aggregate(datasources[i], cally)
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
    if(!(operation %in% ops)){
      stop("'operation' must be set to: 'rowSums', 'colSums', 'rowMeans' or 'colMeans'")
    }
  }
  
  # operation to carry out
  indx <- which(ops == operation)
  
  # create a name by default if user did not provide a name for the new variable
  if(is.null(newobj)){
    newobj <- "rowColCalc_out"
  }
  
  # call the server side function that does the job
  cally <-  paste0("rowColCalcDS(", x, ",", indx, ")")
  opal::datashield.assign(datasources, newobj, as.symbol(cally))
  
  # check that the new object has been created and display a message accordingly
  finalcheck <- isAssigned(datasources, newobj)
  
}
