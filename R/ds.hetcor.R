#' 
#' @title Heterogeneous Correlation Matrix
#' @description This function is based on the hetcor function from the R package \code{polycor}.
#' @details Computes a heterogenous correlation matrix, consisting of Pearson product-moment
#' correlations between numeric variables, polyserial correlations between numeric and ordinal
#' variables, and polychoric correlations between ordinal variables.
#' @param data the name of a data frame consisting of factors, ordered factors, logical variables,
#' character variables, and/or numeric variables, or the first of several variables.
#' @param ML if TRUE, compute maximum-likelihood estimates; if FALSE (default), compute quick
#' two-step estimates.
#' @param std.err if TRUE (default), compute standard errors.
#' @param bins number of bins to use for continuous variables in testing bivariate normality;
#' the default is 4.
#' @param pd if TRUE (default) and if the correlation matrix is not positive-definite, an attempt
#' will be made to adjust it to a positive-definite matrix, using the nearPD function in the Matrix
#' package. Note that default arguments to nearPD are used (except corr=TRUE); for more control call
#' nearPD directly.
#' @param use if "complete.obs", remove observations with any missing data; if "pairwise.complete.obs",
#' compute each correlation using all observations with valid data for that pair of variables.
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} objects obtained after login. 
#' If the \code{datasources} argument is not specified the default set of connections will be
#' used: see \code{\link[DSI]{datashield.connections_default}}.
#' @return Returns an object of class "hetcor" from each study, with the following components: the
#' correlation matrix; the type of each correlation: "Pearson", "Polychoric", or "Polyserial"; the
#' standard errors of the correlations, if requested; the number (or numbers) of observations on which
#' the correlations are based; p-values for tests of bivariate normality for each pair of variables; 
#' the method by which any missing data were handled: "complete.obs" or "pairwise.complete.obs"; TRUE 
#' for ML estimates, FALSE for two-step estimates.
#' @author Demetris Avraam for DataSHIELD Development Team
#' @export
#' 
ds.hetcor <- function(data=NULL, ML=TRUE, std.err=TRUE, bins=4, pd=TRUE, use="complete.obs", datasources=NULL){
  
  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }
  
  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }
  
  if(is.null(data)){
    stop("Please provide the name of the input object!", call.=FALSE)
  }
  
  # check if the input object is defined in all the studies
  defined <- isDefined(datasources, data)
  
  calltext <- call('hetcorDS', data, ML, std.err, bins, pd, use)
  output <- DSI::datashield.aggregate(datasources, calltext)
  
  return(output)
  
}
