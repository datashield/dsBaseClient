#' Compute Weighted Mean by Group
#' 
#' This function is originally from the panelaggregation package.
#' It has been ported here in order to bypass the package being
#' kicked off CRAN.
#' 
#' @author Matthias Bannert, Gabriel Bucur
#' @param data_table a data.table
#' @param variables character name of the variable(s) to focus on. The variables must be in the data.table
#' @param weight character name of the data.table column that contains a weight. 
#' @param by character vector of the columns to group by
#' @return Returns a data table object with computed weighted means.
#'
#' @import data.table
#' @importFrom stats as.formula na.omit ts weighted.mean
#' @keywords internal
computeWeightedMeans <- function(data_table, variables, weight, by) {
  
  if (is.null(weight)) {
    res_dt <- data_table[, lapply(.SD, mean, na.rm = TRUE), .SDcols = variables, by = by]
  } else {
    res_dt <- data_table[, lapply(.SD, weighted.mean, weight = eval(as.name(weight)), na.rm = TRUE),
                         .SDcols = variables, by = by]
  }
  
  res_dt
}
