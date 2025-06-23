#' @title Draw boxplot with information from a data frame
#' 
#' @description Draws a boxplot with the option of adding two grouping variables from data held on a table
#'
#' @param x \code{character} Name of the table on the server side that holds the information to be plotted
#' @param variables \code{character vector} Name of the column(s) of the data frame to include on the boxplot
#' @param group \code{character} (default \code{NULL}) Name of the first grouping variable. 
#' @param group2 \code{character} (default \code{NULL}) Name of the second grouping variable. 
#' @param xlabel \code{caracter} (default \code{"x axis"}) Label to put on the x axis of the plot
#' @param ylabel \code{caracter} (default \code{"y axis"}) Label to put on the y axis of the plot
#' @param type \code{character} Return a pooled plot (\code{"pooled"}) or a split plot (one for each study server
#' \code{"split"})
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} (default \code{NULL}) objects obtained after login
#'
#' @return \code{ggplot} object

ds.boxPlotGG_table <- function(x, variables, group = NULL, group2 = NULL, xlabel = "x axis", 
                               ylabel = "y axis", type = "pooled", datasources = NULL){
  
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  
  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }

  ds.boxPlotGG_data_Treatment(x, variables, group, group2, datasources)
  
  ds.boxPlotGG("boxPlotRawData", group, group2, xlabel, ylabel, type, datasources)
  
}
