#' @title Take a vector on the server side an arrange it to pass it to the boxplot function
#' 
#' @description Internal function
#'
#' @param vector \code{character} Name of the table on the server side that holds the information to be plotted later
#' @param datasources a list of \code{\link{DSConnection-class}} (default \code{NULL}) objects obtained after login
#'
#' @return Does not return nothing, it creates the table \code{"boxPlotRawDataNumeric"} on the server arranged to be passed to the
#' ggplot boxplot function. Structure of the created table: \cr
#' 
#'  Column 'x': Names on the X axis of the boxplot, aka name of the vector (vector argument) \cr
#'  Column 'value': Values for that variable \cr
#'

ds.boxPlotGG_data_Treatment_numeric <- function(vector, datasources = NULL){
  
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  
  cally <- paste0("boxPlotGG_data_Treatment_numericDS(", vector, ")")
  DSI::datashield.assign.expr(datasources, "boxPlotRawDataNumeric", as.symbol(cally))
  
}
