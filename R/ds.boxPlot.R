#' @title Draw boxplot
#' 
#' @description Draw boxplot with data on the study servers (data frames or numeric vectors) with the option
#' of grouping using categorical variables on the dataset (only for data frames)
#'
#' @param x \code{character} Name of the data frame (or numeric vector) on the server side that
#'  holds the information to be plotted
#' @param variables \code{character vector} Name of the column(s) of the data frame to include on the boxplot
#' @param group \code{character} (default \code{NULL}) Name of the first grouping variable. 
#' @param group2 \code{character} (default \code{NULL}) Name of the second grouping variable. 
#' @param xlabel \code{caracter} (default \code{"x axis"}) Label to put on the x axis of the plot
#' @param ylabel \code{caracter} (default \code{"y axis"}) Label to put on the y axis of the plot
#' @param type \code{character} Return a pooled plot (\code{"pooled"}) or a split plot (one for each study server
#' \code{"split"})
#' @param datasources a list of \code{\link{DSConnection-class}} (default \code{NULL}) objects obtained after login
#'
#' @return \code{ggplot} object
#' @export

ds.boxPlot <- function(x, variables = NULL, group = NULL, group2 = NULL, xlabel = "x axis", 
                       ylabel = "y axis", type = "pooled", datasources = NULL){
  
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  
  isDefined(datasources, x)
  cls <- checkClass(datasources, x)
  
  if(any(cls %in% c("data.frame"))){
    ds.boxPlotGG_table(x, variables, group, group2, xlabel, ylabel, type, datasources)
  }
  else if(any(cls %in% c("numeric"))){
    ds.boxPlotGG_numeric(x, xlabel, ylabel, type, datasources)
  }
  else(stop("The selected object is not a data frame nor a numerical vector"))
  
}
