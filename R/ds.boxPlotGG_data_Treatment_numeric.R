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

ds.boxPlotGG_data_Treatment_numeric <- function(vector, datasources = NULL){
  
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }

  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }

  cally <- paste0("boxPlotGG_data_Treatment_numericDS(", vector, ")")
  DSI::datashield.assign.expr(datasources, "boxPlotRawDataNumeric", as.symbol(cally))
  
}
