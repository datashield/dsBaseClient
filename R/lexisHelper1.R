#' 
#' @title Generates a default value to use as interval width
#' @description This is an internal function required by the
#' function \code{ds.lexis}
#' @details If the analyst has not provided an interval width
#' used to generate the expanded table in a piecewise exponential
#' regression, this functions generates a default value which is
#' 1/10 of the global mean of the exit time values.
#' @param datasources a list of opal object(s) obtained after login to opal servers;
#' these objects also hold the data assigned to R, as a \code{data frame}, from opal 
#' datasources
#' @param exitTime a character, the name of the column that holds the exit times.
#' @keywords internal
#' @return a numeric value
#'
lexisHelper1 <-  function(datasources, exitTime){
  
  # get the global mean
  cally <- paste0("meanDS(", exitTime, ")")
  mean.local <- datashield.aggregate(datasources, as.symbol(cally))
  cally <- paste0("NROW(", exitTime, ")")
  length.local <- datashield.aggregate(datasources, cally)
  length.total = 0
  sum.weighted = 0
  mean.global  = NA
  for (i in 1:length(datasources)){
    if ((!is.null(length.local[[i]])) & (length.local[[i]]!=0)) {
      length.total = length.total + length.local[[i]]
      sum.weighted = sum.weighted + length.local[[i]] * mean.local[[i]]
    }
  }
  globalMean = sum.weighted/length.total  
  
  # return the interval width to use
  defaultInterval <-  globalMean/10
  return(defaultInterval)
}