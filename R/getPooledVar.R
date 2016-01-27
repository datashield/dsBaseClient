#' 
#' @title Gets a pooled variance
#' @description This is an internal function.
#' @details This function is called to avoid calling the client function 'ds.var' 
#' which may stop the process due to some checks not required when computing a mean inside 
#' a function.
#' @param dtsources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @param x a character, the name of a numeric vector
#' @keywords internal
#' @return a pooled variance
#'
getPooledVar <- function(dtsources, x){
  
  num.sources <- length(dtsources)
  
  cally <- paste0("varDS(", x, ")")
  var.local <- opal::datashield.aggregate(dtsources, as.symbol(cally))
  
  cally <- paste0("NROW(", x, ")")
  length.local <- opal::datashield.aggregate(dtsources, cally)

  # get the number of entries with missing values
  cally <- paste0("numNaDS(", x, ")")
  numNA.local <- opal::datashield.aggregate(dtsources, cally)

  length.total = 0
  sum.weighted = 0
  var.global  = NA
  
  for (i in 1:num.sources){
    if ((!is.null(length.local[[i]])) & (length.local[[i]]!=0)) {
      completeLength <- length.local[[i]]-numNA.local[[i]]
      length.total = length.total+completeLength
      sum.weighted = sum.weighted+completeLength*var.local[[i]]
    }
  }
  
  var.global = sum.weighted/length.total
  return(var.global)
  
}