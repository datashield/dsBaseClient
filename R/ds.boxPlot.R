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
#' @examples 
#' \dontrun{
#'   ## Version 6, for version 5 see the Wiki
#'   
#'   ### Please ensure you have a training Virtual Machine running,
#'    or that you have a live connection to a server.
#'      
#'   # Connecting to the Opal servers
#' 
#'   require('DSI')
#'   require('DSOpal')
#'   require('dsBaseClient')
#'
#'   builder <- DSI::newDSLoginBuilder()
#'   builder$append(server = "study1", 
#'                  url = "http://192.168.56.100:8080/", 
#'                  user = "administrator", password = "datashield_test&", 
#'                  table = "CNSIM.CNSIM1", driver = "OpalDriver")
#'   builder$append(server = "study2", 
#'                  url = "http://192.168.56.100:8080/", 
#'                  user = "administrator", password = "datashield_test&", 
#'                  table = "CNSIM.CNSIM2", driver = "OpalDriver")
#'   builder$append(server = "study3",
#'                  url = "http://192.168.56.100:8080/", 
#'                  user = "administrator", password = "datashield_test&", 
#'                  table = "CNSIM.CNSIM3", driver = "OpalDriver")
#'   logindata <- builder$build()
#'   
#'   connections <- DSI::datashield.login(logins = logindata, assign = TRUE,
#'    symbol = "D") 
#'   
#'   ## Create a boxplot of one variable
#'   ds.boxPlot("D", "LAB_HDL", datasources = connections) 
#'  
#'   ## Create a boxplot that is split by study:
#'   ds.boxPlot("D", "LAB_HDL", type= "split", datasources = connections) 
#'   
#'   ## Create a boxplot of two variables variable
#'   ds.boxPlot("D", c("LAB_HDL", "LAB_TRIG", type="combine", 
#'   datasources = connections)  
#'   # only one plot is created (of the aggregated results of all servers)
#'   
#'   ## Create a boxplot of two variables, which are split by a factor
#'   ds.boxPlot("D", c("LAB_HDL", "LAB_TRIG"), group = "GENDER", 
#'   datasources = connections)
#'   
#'   ## Create a boxplot with x- and y-axis labels
#'   ds.boxPlot("D", c("LAB_HDL", "LAB_TRIG"), group = "GENDER", 
#'   xlabel = "Variable", ylabel = "Measurement", datasources = connections)
#'   
#'   ## Improve the presentation of ds.boxplot output using ggplot:
#'   ### User must save the output, which is in a ggplot format already:
#'   a <- ds.boxPlot("D", c("LAB_HDL", "LAB_TRIG"), group = "GENDER", 
#'   xlabel = "Variable", ylabel = "Measurement", datasources = connections)
#'                  
#'   ### Then customise output "a" using ggplot tools:
#'   a + ggplot2::scale_fill_discrete(name = "Gender", labels = c("Male", "Female"))
#'   
#'   ### Or use an alternative way, to maintain the aesthetics:
#'   a + ggplot2::scale_fill_brewer(name = "Gender", labels = c("Male", "Female")) 
#'                                                                                 
#'   # Clear the Datashield R sessions and logout                 
#'   datashield.logout(connections) 
#'   
#' }   
#'

ds.boxPlot <- function(x, variables = NULL, group = NULL, group2 = NULL, xlabel = "x axis", 
                       ylabel = "y axis", type = "pooled", datasources = NULL){
  
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }

  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
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
