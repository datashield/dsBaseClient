#' @title Take a data frame on the server side an arrange it to pass it to the boxplot function
#' 
#' @description Internal function
#'
#' @param table \code{character} Name of the table on the server side that holds the information to be plotted later
#' @param variables \code{character vector} Name of the column(s) of the data frame to include on the boxplot
#' @param group \code{character} (default \code{NULL}) Name of the first grouping variable. 
#' @param group2 \code{character} (default \code{NULL}) Name of the second grouping variable. 
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} (default \code{NULL}) objects obtained after login
#'
#' @return Does not return nothing, it creates the table \code{"boxPlotRawData"} on the server arranged to be passed to the
#' ggplot boxplot function. Structure of the created table: \cr
#' 
#'  Column 'x': Names on the X axis of the boxplot, aka variables to plot \cr
#'  Column 'value': Values for that variable (raw data of columns rbinded) \cr
#'  Column 'group': (Optional) Values of the grouping variable \cr
#'  Column 'group2': (Optional) Values of the second grouping variable \cr
#'

ds.boxPlotGG_data_Treatment <- function(table, variables, group = NULL, group2 = NULL, datasources = NULL){
  
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }

  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }

  cally <- paste0("boxPlotGG_data_TreatmentDS(", table, ", c('", 
                  paste0(variables, collapse = "','"), "'), ", 
                  if(is.null(group)){paste0("NULL")}else{paste0("'",group,"'")}, ", ", 
                  if(is.null(group2)){paste0("NULL")}else{paste0("'",group2,"'")}, ")")
  DSI::datashield.assign.expr(datasources, "boxPlotRawData", as.symbol(cally))
  
  
}
