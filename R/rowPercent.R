#'
#' @title Produces row percentages
#' @description this is an INTERNAL function.
#' @details The function is required required by the client function \code{ds.table2D}. 
#' @param dataframe a data frame
#' @keywords internal
#' @return a data frame
#' @author Gaye A
#' 
rowPercent <- function(dataframe){
  dt <- dataframe
  lastrow <- dim(dataframe)[1]
  lastcol <- dim(dataframe)[2]
  for(i in 1:lastrow){
    totalval <- dataframe[i, lastcol]
    dt[i,] <- round((dataframe[i,]/totalval)*100,2)      
  }
  return(dt)
}



