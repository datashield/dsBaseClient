#' @title Take a vector on the server side an arrange it to pass it to the boxplot function
#' 
#' @description Internal function
#'
#' @param vector \code{character} Name of the table on the server side that holds the information to be plotted later
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} (default \code{NULL}) objects obtained after login
#'
#' @return Does not return nothing, it creates the table \code{"boxPlotRawDataNumeric"} on the server arranged to be passed to the
#' ggplot boxplot function. Structure of the created table: \cr
#' 
#'  Column 'x': Names on the X axis of the boxplot, aka name of the vector (vector argument) \cr
#'  Column 'value': Values for that variable \cr
#'
#' @author Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands

ds.boxPlotGG_data_Treatment_numeric <- function(vector, datasources = NULL){
  
  datasources <- .set_datasources(datasources)

  datashield.assign.expr(datasources, "boxPlotRawDataNumeric", call("boxPlotGG_data_Treatment_numericDS", vector.name = vector))
  
}
