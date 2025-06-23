#' @title Draw boxplot with information from a numeric vector
#'
#' @param x \code{character} Name of the numeric vector on the server side that holds the information to be plotted
#' @param xlabel \code{caracter} (default \code{"x axis"}) Label to put on the x axis of the plot
#' @param ylabel \code{caracter} (default \code{"y axis"}) Label to put on the y axis of the plot
#' @param type \code{character} Return a pooled plot (\code{"pooled"}) or a split plot (one for each study server
#' \code{"split"})
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} (default \code{NULL}) objects obtained after login
#'
#' @return \code{ggplot} object

ds.boxPlotGG_numeric <- function(x, xlabel = "x axis", ylabel = "y axis", type = "pooled", datasources = NULL){
  
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }

  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }

  ds.boxPlotGG_data_Treatment_numeric(x, datasources)
  
  ds.boxPlotGG("boxPlotRawDataNumeric", NULL, NULL, xlabel, ylabel, type, datasources)
  
}
