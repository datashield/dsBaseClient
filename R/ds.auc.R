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
#' @author Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
#' @export
#'
ds.auc <- function(pred=NULL, y=NULL, datasources=NULL){
  
  datasources <- .set_datasources(datasources)
  
  # verify that 'pred' was set
  if(is.null(pred)){
    stop("Please provide the name of the vector with predicted values", call.=FALSE)
  }
  
  # verify that 'y' was set
  if(is.null(y)){
    stop("Please provide the name of the outcome variable", call.=FALSE)
  }
  
  cally <- call('aucDS', pred, y)
  output <- DSI::datashield.aggregate(datasources, cally)
  
  return(output)
  
}
