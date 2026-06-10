#' 
#' @title Basis for a piecewise linear spline with meaningful coefficients
#' @description This function is based on the native R function \code{lspline} from the
#' \code{lspline} package. This function computes the basis of piecewise-linear spline
#' such that, depending on the argument marginal, the coefficients can be interpreted as
#' (1) slopes of consecutive spline segments, or (2) slope change at consecutive knots.
#' @details If marginal is FALSE (default) the coefficients of the spline correspond to
#' slopes of the consecutive segments. If it is TRUE the first coefficient correspond to
#' the slope of the first segment. The consecutive coefficients correspond to the change
#' in slope as compared to the previous segment.
#' @param x the name of the input numeric variable
#' @param knots numeric vector of knot positions
#' @param marginal logical, how to parametrise the spline, see Details
#' @param names character, vector of names for constructed variables
#' @param newobj a character string that provides the name for the output 
#' variable that is stored on the data servers. Default \code{lspline.newobj}. 
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} 
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#' @return an object of class "lspline" and "matrix", which its name is specified by the
#' \code{newobj} argument (or its default name "lspline.newobj"), is assigned on the serverside.
#' @author Demetris Avraam for DataSHIELD Development Team
#' @export
#'
ds.lspline <- function(x, knots = NULL, marginal = FALSE, names = NULL, newobj = NULL, datasources = NULL){
  
  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }
  
  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }
  
  if(is.null(x)){
    stop("Please provide the name of the input variable x!", call.=FALSE)
  }
  
  # check if the input object is defined in all the studies
  defined <- isDefined(datasources, x)
  
  if(is.null(knots)){
    stop("Please provide a vector of knots!", call.=FALSE)
  }
  
  # create a name by default if user did not provide a name for the new variable
  if(is.null(newobj)){
    newobj <- "lspline.newobj"
  }
  
  # now do the business
  calltext <- call("lsplineDS", x, knots, marginal, names)
  DSI::datashield.assign(datasources, newobj, calltext)
  
}
