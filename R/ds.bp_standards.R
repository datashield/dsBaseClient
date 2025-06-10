#' 
#' @title Calculates Blood pressure z-scores
#' @description The function calculates blood pressure z-scores in two steps:
#' Step 1. Calculates z-score of height according to CDC growth chart (Not the 
#' WHO growth chart!). Step 2. Calculates z-score of BP according to the fourth
#' report on BP management, USA
#' @param sex the name of the sex variable. The variable should be coded as 1 for males
#' and 2 for females. If it is coded differently (e.g. 0/1), then you can use the 
#' ds.recodeValues function to recode the categories to 1/2 before the use of 
#' ds.bp_standards
#' @param age the name of the age variable in years.
#' @param height the name of the height variable in cm.
#' @param bp the name of the blood pressure variable.
#' @param systolic logical. If TRUE (default) the function assumes conversion of 
#' systolic blood pressure. If FALSE the function assumes conversion of diastolic 
#' blood pressure.
#' @param newobj a character string that provides the name for the output object
#' that is stored on the data servers. Default name is set to \code{bp.newobj}. 
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} objects obtained after login. 
#' If the \code{datasources} argument is not specified the default set of connections will be
#' used: see \code{\link[DSI]{datashield.connections_default}}.
#' @return assigns a new object on the server-side. The assigned object is a list 
#' with two elements: the 'Zbp' which is the zscores of the blood pressure and 'perc'
#' which is the percentiles of the BP zscores.
#' @references The fourth report on the diagnosis, evaluation, and treatment of high
#' blood pressure in children and adolescents:
#' https://www.nhlbi.nih.gov/sites/default/files/media/docs/hbp_ped.pdf
#' @author Demetris Avraam for DataSHIELD Development Team
#' @import data.table
#' @export
ds.bp_standards <- function(sex=NULL, age=NULL, height=NULL, bp=NULL, systolic=TRUE, 
                            newobj=NULL, datasources=NULL){
  
  # look for DS connections
  if(is.null(datasources)){
    datasources <- DSI::datashield.connections_find()
  }
  
  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }
  
  # verify that 'sex' was set
  if(is.null(sex)){
    stop("Please provide the name of the sex variable", call.=FALSE)
  }
  
  # verify that 'age' was set
  if(is.null(age)){
    stop("Please provide the name of the age in months variable", call.=FALSE)
  }
  
  # verify that 'height' was set
  if(is.null(height)){
    stop("Please provide the name of the height in cm variable", call.=FALSE)
  }
  
  # verify that 'bp' was set
  if(is.null(bp)){
    stop("Please provide the name of the blood pressure variable", call.=FALSE)
  }
  
  # check if the input objects are defined in all the studies
  defined.sex <- isDefined(datasources, sex)
  defined.age <- isDefined(datasources, age)
  defined.height <- isDefined(datasources, height)
  defined.bp <- isDefined(datasources, bp)
  
  # if no output object specified then provide a default name
  if(is.null(newobj)){
    newobj <- "bp.newobj"
  }
  
  # call the server-side assign function
  cally <- call('bp_standardsDS', sex, age, height, bp, systolic)
  DSI::datashield.assign(datasources, newobj, cally)
  
}
