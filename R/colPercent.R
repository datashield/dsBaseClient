#'
#' @title Produces column percentages
#' @description this is an INTERNAL function.
#' @details The function is required required by the client function \code{ds.table2D}. 
#' @param dataframe a data frame
#' @keywords internal
#' @return a data frame
#' @author Gaye, A.
#' 
colPercent <- function(dataframe){
  dt <- dataframe
  lastcol <- dim(dataframe)[2]
  lastrow <- dim(dataframe)[1]
  for(i in 1:lastcol){
    totalval <- dataframe[lastrow,i]
    dt[,i] <- round((dataframe[,i]/totalval)*100,2)      
  }
  return(dt)
}



