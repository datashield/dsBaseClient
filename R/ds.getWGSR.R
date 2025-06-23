#'
#' @title Computes the WHO Growth Reference z-scores of anthropometric data 
#' @description Calculate WHO Growth Reference z-score for a given anthropometric measurement
#' This function is similar to R function \code{getWGSR} from the \code{zscorer} package.
#' @details The function calls the server-side function \code{getWGSRDS} that computes the WHO 
#' Growth Reference z-scores of anthropometric data for weight, height or length, MUAC (middle
#' upper arm circumference), head circumference, sub-scapular skinfold and triceps skinfold.
#' Note that the function might fail or return NAs when the variables are outside the ranges
#' given in the WGS (WHO Child Growth Standards) reference (i.e. 45 to 120 cm for height and
#' 0 to 60 months for age). It is up to the user to check the ranges and the units of their 
#' data.
#' @param sex the name of the binary variable that indicates the sex of the subject. This must
#' be coded as 1 = male and 2 = female. If in your project the variable sex has different
#' levels, you should recode the levels to 1 for males and 2 for females using the 
#' \code{ds.recodeValues} DataSHIELD function before the use of the \code{ds.getWGSR}.
#' @param firstPart Name of variable specifying:\cr
#' Weight (kg) for BMI/A, W/A, W/H, or W/L\cr
#' Head circumference (cm) for HC/A\cr
#' Height (cm) for H/A\cr
#' Length (cm) for L/A\cr
#' MUAC (cm) for MUAC/A\cr
#' Sub-scapular skinfold (mm) for SSF/A\cr
#' Triceps skinfold (mm) for TSF/A\cr
#' Give a quoted variable name as in (e.g.) "weight". Be careful with units (weight in kg;
#' height, length, head circumference, and MUAC in cm; skinfolds in mm).
#' @param secondPart Name of variable specifying:\cr
#' Age (days) for H/A, HC/A, L/A, MUAC/A, SSF/A, or TSF/A\cr
#' Height (cm) for BMI/A, or W/H\cr
#' Length (cm) for W/L\cr
#' Give a quoted variable name as in (e.g.) "age". Be careful with units (age in days;
#' height and length in cm).
#' @param index The index to be calculated and added to data. One of:\cr
#' bfa BMI for age\cr
#' hca Head circumference for age\cr
#' hfa Height for age\cr
#' lfa Length for age\cr
#' mfa MUAC for age\cr
#' ssa Sub-scapular skinfold for age\cr
#' tsa Triceps skinfold for age\cr
#' wfa Weight for age\cr
#' wfh Weight for height\cr
#' wfl Weight for length\cr
#' Give a quoted index name as in (e.g.) "wfh".
#' @param standing Variable specifying how stature was measured. If NA (default) then age (for "hfa"
#' or "lfa") or height rules (for "wfh" or "wfl") will be applied. This must be coded as
#' 1 = Standing; 2 = Supine; 3 = Unknown. Missing values will be recoded to 3 = Unknown. 
#' Give a single value (e.g."1"). If no value is specified then height and age rules will be applied.
#' @param thirdPart Name of variable specifying age (in days) for BMI/A. Give a quoted variable
#' name as in (e.g.) "age". Be careful with units (age in days). If age is given in different units
#' you should convert it in age in days using the \code{ds.make} DataSHIELD function before the use
#' of the \code{ds.getWGSR}. For example if age is given in months then the transformation is given
#' by the formula $age_days=age_months*(365.25/12)$. 
#' @param newobj a character string that provides the name for the output variable 
#' that is stored on the data servers. Defaults \code{getWGSR.newobj}. 
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} objects obtained after login. 
#' If the \code{datasources} argument is not specified the default set of connections will be
#' used: see \code{\link[DSI]{datashield.connections_default}}.
#' @return \code{ds.getWGSR} assigns a vector for each study that includes the z-scores for the
#' specified index. The created vectors are stored in the servers.
#' @author Demetris Avraam for DataSHIELD Development Team
#' @export
#' @examples
#' \dontrun{
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
#'                  table = "ANTHRO.anthro1", driver = "OpalDriver")
#'   builder$append(server = "study2", 
#'                  url = "http://192.168.56.100:8080/", 
#'                  user = "administrator", password = "datashield_test&", 
#'                  table = "ANTHRO.anthro2", driver = "OpalDriver")
#'   builder$append(server = "study3",
#'                  url = "http://192.168.56.100:8080/", 
#'                  user = "administrator", password = "datashield_test&", 
#'                  table = "ANTHRO.anthro3", driver = "OpalDriver")
#'                  
#'   logindata <- builder$build()
#'   
#'   # Log onto the remote Opal training servers
#'   connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D") 
#'   
#'   # Example 1: Generate the weight-for-height (wfh) index
#'   ds.getWGSR(sex = "D$sex", firstPart = "D$weight", secondPart = "D$height",
#'            index = "wfh", newobj = "wfh_index", datasources = connections)
#'
#'   # Example 2: Generate the BMI for age (bfa) index
#'   ds.getWGSR(sex = "D$sex", firstPart = "D$weight", secondPart = "D$height",
#'            index = "bfa", thirdPart = "D$age", newobj = "bfa_index", datasources = connections)
#'
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(connections) 
#'
#' }
#'
ds.getWGSR <- function(sex=NULL, firstPart=NULL, secondPart=NULL, index=NULL, standing=NA, thirdPart=NA, newobj=NULL, datasources=NULL){
  
  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }

  if(is.null(sex)){
    stop("Please provide the column name of the 'sex' variable!", call.=FALSE)
  }
  
  if(is.null(firstPart)){
    stop("Please provide the column name of the 'firstPart' variable!", call.=FALSE)
  }
  
  if(is.null(secondPart)){
    stop("Please provide the column name of the 'secondPart' variable!", call.=FALSE)
  }
  
  # check if the input objects are defined in all the studies
  isDefined(datasources, sex)
  isDefined(datasources, firstPart)
  isDefined(datasources, secondPart)
  
  # if 'firstPart' or 'secondPart' are not numeric return an error message
  typ.firstPart <- checkClass(datasources, firstPart)
  typ.secondPart <- checkClass(datasources, secondPart)
  if(!('numeric' %in% typ.firstPart)){
    stop("The 'firstPart' variable must be a 'numeric' variable!", call.=FALSE)
  }
  if(!('numeric' %in% typ.secondPart)){
    stop("The 'secondPart' variable must be a 'numeric' variable!", call.=FALSE)
  }
  
  if(!any(index %in% c("bfa", "hca", "hfa", "lfa", "mfa", "ssa", "tsa", "wfa", "wfh", "wfl"))){
    stop("Please provide a correct abbreviation for the index!", call.=FALSE)
  }
  
  # If 'thirdPart' (age) is missing for BMI-for-age return an error message
  if(index == "bfa" & is.na(thirdPart)) {
    stop("'thirdPart' variable should not be missing for index 'bfa'", call.=FALSE)
  }
  
  # If 'thirdPart' (age) is not numeric for BMI-for-age return an error message
  if(index == "bfa"){
    typ.thirdPart <- checkClass(datasources, thirdPart)
    if(!('numeric' %in% typ.firstPart)){
      stop("The 'thirdPart' variable must be a 'numeric' variable!", call.=FALSE)
    }  
  }
  
  # If 'standing' is not a value either 1, 2, 3, or NA return an error message
  if(!any(standing %in% c(NA, 1, 2, 3))) {
    stop("The 'standing' variable must be a numeric value either 1, 2, or 3!", call.=FALSE)
  }
  
  # check newobj not actively declared as null
  if(is.null(newobj)){
    newobj <- "getWGSR.newobj"
  }
  
  cally <- call("getWGSRDS", sex, firstPart, secondPart, index, standing, thirdPart)
  DSI::datashield.assign(datasources, newobj, cally)
  
  #############################################################################################################
  # DataSHIELD CLIENTSIDE MODULE: CHECK KEY DATA OBJECTS SUCCESSFULLY CREATED  
  
  # SET APPROPRIATE PARAMETERS FOR THIS PARTICULAR FUNCTION 
  test.obj.name <- newobj	
  
  # CALL SEVERSIDE FUNCTION
  calltext <- call("testObjExistsDS", test.obj.name)
  object.info <- DSI::datashield.aggregate(datasources, calltext)
  
  # CHECK IN EACH SOURCE WHETHER OBJECT NAME EXISTS
  # AND WHETHER OBJECT PHYSICALLY EXISTS WITH A NON-NULL CLASS
  num.datasources <- length(object.info)
  
  obj.name.exists.in.all.sources <- TRUE
  obj.non.null.in.all.sources <- TRUE
  
  for(j in 1:num.datasources){
    if(!object.info[[j]]$test.obj.exists){
      obj.name.exists.in.all.sources <- FALSE
    }
    if(is.null(object.info[[j]]$test.obj.class) || ("ABSENT" %in% object.info[[j]]$test.obj.class)){
      obj.non.null.in.all.sources <- FALSE
    }
  }
  
  if(obj.name.exists.in.all.sources && obj.non.null.in.all.sources){
    return.message <- paste0("A data object <", test.obj.name, "> has been created in all specified data sources")
  }else{
    return.message.1 <- paste0("Error: A valid data object <", test.obj.name, "> does NOT exist in ALL specified data sources")
    return.message.2 <- paste0("It is either ABSENT and/or has no valid content/class,see return.info above")	
    return.message.3 <-	paste0("Please use ds.ls() to identify where missing")
    return.message <- list(return.message.1,return.message.2,return.message.3)
  }
  
  calltext <- call("messageDS", test.obj.name)
  studyside.message <- DSI::datashield.aggregate(datasources, calltext)
  no.errors <- TRUE
  for(nd in 1:num.datasources){
    if(studyside.message[[nd]]!="ALL OK: there are no studysideMessage(s) on this datasource"){
      no.errors <- FALSE
    }
  }
  
  if(no.errors){
    validity.check <- paste0("<",test.obj.name, "> appears valid in all sources")
    return(list(is.object.created=return.message,validity.check=validity.check))
  }
  
  if(!no.errors){
    validity.check <- paste0("<",test.obj.name,"> invalid in at least one source. See studyside.messages:")
    return(list(is.object.created=return.message,validity.check=validity.check,
                studyside.messages=studyside.message))
  }
  
  # END OF CHECK OBJECT CREATED CORECTLY MODULE	
  #######################################################################################################

}  
