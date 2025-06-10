#'
#' @title Calculates the Area under the curve (AUC)
#' @description This function calculates the C-statistic or AUC
#' for logistic regression models.
#' @details The AUC determines the discriminative ability of a model.
#' @param pred the name of the vector of the predicted values
#' @param y the name of the outcome variable. Note that this variable should include 
#' the complete cases that are used in the regression model.
#' @param datasources  a list of \code{\link[DSI]{DSConnection-class}} 
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#' @return returns the AUC and its standard error
#' @author Demetris Avraam for DataSHIELD Development Team
#' @export
#'
ds.auc <- function(pred=NULL, y=NULL, datasources=NULL){
  
  # look for DS connections
  if(is.null(datasources)){
    datasources <- DSI::datashield.connections_find()
  }
  
  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }
  
  # verify that 'pred' was set
  if(is.null(pred)){
    stop("Please provide the name of the vector with predicted values", call.=FALSE)
  }
  
  # verify that 'y' was set
  if(is.null(y)){
    stop("Please provide the name of the outcome variable", call.=FALSE)
  }
  
  # check if the pred and y objects are defined in all the studies
  defined.pred <- isDefined(datasources, pred)
  defined.y <- isDefined(datasources, y)
  
  cally <- call('aucDS', pred, y)
  output <- DSI::datashield.aggregate(datasources, cally)
  
  return(output)
  
}
