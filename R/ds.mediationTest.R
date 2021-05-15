#'
#' @title The Sobel mediation test
#' @description This function is similar to the R function \code{mediation.test} that is included
#' in the \code{bda} package. The function computes statistics and p-values for three versions of
#' Sobel mediation test: Sobel test, Aroian test and Goodman test.
#' @details To test whether a mediator carries the influence on an independent variable to a 
#' dependent variable. Missing values will be automatically excluded with a warning.
#' @param mv a string character, the name of the mediator variable
#' @param iv a string character, the name of the independent variable
#' @param dv a string character, the name of the dependent variable
#' @param datasources a list of \code{\link{DSConnection-class}} 
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link{datashield.connections_default}}.
#' @return A table showing the values of the test statistics (z-values) and the corresponding
#' p-values for the Sobel, Aroian and Goodman tests for each study separately. 
#' @author Demetris Avraam for DataSHIELD Development Team
#' @export
#' 
ds.mediationTest <- function(mv=NULL, iv=NULL, dv=NULL, datasources=NULL){
  
  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }
  
  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }
  
  if(is.null(mv)){
    stop("Please provide the name of the mediator variable!", call.=FALSE)
  }
  
  if(is.null(iv)){
    stop("Please provide the name of the independent variable!", call.=FALSE)
  }
  
  if(is.null(dv)){
    stop("Please provide the name of the dependent variable!", call.=FALSE)
  }
  
  # the input variables might be given as column table (i.e. D$x)
  # or just as vectors not attached to a table (i.e. x)
  # we have to make sure the function deals with each case
  mv.names <- extract(mv)
  mv.varname <- mv.names$elements
  mv.obj2lookfor <- mv.names$holders
  
  iv.names <- extract(iv)
  iv.varname <- iv.names$elements
  iv.obj2lookfor <- iv.names$holders
  
  dv.names <- extract(dv)
  dv.varname <- dv.names$elements
  dv.obj2lookfor <- dv.names$holders
  
  # check if the input objects are defined in all the studies
  if(is.na(mv.obj2lookfor)){
    mv.defined <- isDefined(datasources, mv.varname)
  }else{
    mv.defined <- isDefined(datasources, mv.obj2lookfor)
  }
  
  if(is.na(iv.obj2lookfor)){
    iv.defined <- isDefined(datasources, iv.varname)
  }else{
    iv.defined <- isDefined(datasources, iv.obj2lookfor)
  }
  
  if(is.na(mv.obj2lookfor)){
    dv.defined <- isDefined(datasources, dv.varname)
  }else{
    dv.defined <- isDefined(datasources, dv.obj2lookfor)
  }
  
  # call the internal function that checks the input objects are of the same class in all studies.
  mv.typ <- checkClass(datasources, mv)
  iv.typ <- checkClass(datasources, iv)
  dv.typ <- checkClass(datasources, dv)
  
  # the input variables must be either numeric, factor or integers
  if(mv.typ != 'integer' & mv.typ != 'numeric' & mv.typ != 'factor'){
    message(paste0(mv, " is of type ", mv.typ, "!"))
    stop("The mediator variable must be an integer, factor or numeric vector.", call.=FALSE)
  }
  
  if(iv.typ != 'integer' & iv.typ != 'numeric' & iv.typ != 'factor'){
    message(paste0(iv, " is of type ", iv.typ, "!"))
    stop("The independent variable must be an integer, factor or numeric vector.", call.=FALSE)
  }
  
  if(dv.typ != 'integer' & dv.typ != 'numeric' & dv.typ != 'factor'){
    message(paste0(dv, " is of type ", dv.typ, "!"))
    stop("The dependent variable must be an integer, factor or numeric vector.", call.=FALSE)
  }
   
  calltext <- call("mediationTestDS", mv, iv, dv)
  output <- DSI::datashield.aggregate(datasources, calltext)
   
  return(output)
  
}
