#' 
#' @title Gets a pooled statistical mean
#' @description This is an internal function.
#' @details This function is called to avoid calling the client function 'ds.mean' 
#' which may stop the process due to some checks not required when computing a mean inside 
#' a function.
#' @param dtsources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @param x a character, the name of a numeric vector
#' @keywords internal
#' @return a pooled mean
#'
getPooledMean <- function(dtsources, x){
  
  num.sources <- length(dtsources)
  
  cally <- paste0("meanDS(", x, ")")
  mean.local <- datashield.aggregate(dtsources, as.symbol(cally))
  
  cally <- paste0("NROW(", x, ")")
  length.local <- datashield.aggregate(dtsources, cally)

  length.total = 0
  sum.weighted = 0
  mean.global  = NA
  
  for (i in 1:num.sources){
    if ((!is.null(length.local[[i]])) & (length.local[[i]]!=0)) {
      length.total = length.total+length.local[[i]]
      sum.weighted = sum.weighted+length.local[[i]]*mean.local[[i]]
    }
  }
  
  mean.global = sum.weighted/length.total
  return(mean.global)

}