#' @title Recodes server-side variable values
#' @description This function takes specified values of elements in a vector and converts
#' them to a matched set of alternative specified values.
#' @details This function recodes individual values with new individual values. This can
#' apply to numeric and character values, factor levels and NAs. One particular use of
#' \code{ds.recodeValues} is to convert NAs to an explicit value. This value is specified
#' in the argument \code{missing}. If the user want to recode only missing values, then it
#' should also specify an identical vector of values in both arguments \code{values2replace.vector}
#' and \code{new.values.vector} (see Example 2 below).
#' Server function called: \code{recodeValuesDS}
#' @param var.name a character string providing the name of the variable to be recoded. 
#' @param values2replace.vector a numeric or character vector specifying the values
#' in the variable \code{var.name} to be replaced. 
#' @param new.values.vector a numeric or character vector specifying the new values.
#' @param missing If supplied, any missing values in var.name will be replaced by this value. 
#' Must be of length 1. If the analyst want to recode only missing values then it should also 
#' specify an identical vector of values in both arguments \code{values2replace.vector} and 
#' \code{new.values.vector}. Otherwise please look the \code{ds.replaceNA} function.
#' @param newobj a character string that provides the name for the output object
#' that is stored on the data servers.
#' Default \code{recodevalues.newobj}. 
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} 
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#' @param notify.of.progress logical. If TRUE console output should be produced to indicate
#' progress. Default FALSE.
#' @return Assigns to each server a new variable with the recoded values.
#' @author DataSHIELD Development Team
#' @author Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
#' @examples
#' \dontrun{
#' 
#'   ## Version 6, for version 5 see the Wiki
#'   
#'   # connecting to the Armadillo/Opal servers
#' 
#'   require('DSI')
#'   require('DSMolgenisArmadillo')
#'   require('DSOpal')
#'   require('dsBaseClient')
#'
#'   armadillo_url <- "http://192.168.56.100:8080/"
#'   token <- armadillo.get_credentials(armadillo_url)
#'   builder <- DSI::newDSLoginBuilder()
#'   builder$append(server = "study1", 
#'                  url = armadillo_url, 
#'                  token = token@access_token, 
#'                  table = "datashield/cnsim/CNSIM1", driver = "ArmadilloDriver")
#'   builder$append(server = "study2", 
#'                  url = "https://192.168.56.100:8443/", 
#'                  user = "administrator", password = "datashield_test&", 
#'                  table = "CNSIM.CNSIM2", driver = "OpalDriver",
#'                  options = "list(ssl_verifyhost=0, ssl_verifypeer=0)")
#'   logindata <- builder$build()
#'   
#'   connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D") 
#'   
#'   # Example 1: recode the levels of D$GENDER
#'   ds.recodeValues(var.name = "D$GENDER", 
#'                   values2replace.vector = c(0,1), 
#'                   new.values.vector = c(10,20),
#'                   newobj = 'gender_recoded',
#'                   datasources = connections)
#'                   
#'   # Example 2: recode NAs in D$PM_BMI_CATEGORICAL          
#'   ds.recodeValues(var.name = "D$PM_BMI_CATEGORICAL", 
#'                   values2replace.vector = c(1,2), 
#'                   new.values.vector = c(1,2),
#'                   missing = 99, 
#'                   newobj = 'bmi_recoded',
#'                   datasources = connections)
#'                  
#'   # Clear the Datashield R sessions and logout                 
#'   datashield.logout(connections) 
#'   
#' }   
#' @export
#'
ds.recodeValues <- function(var.name=NULL, values2replace.vector=NULL, new.values.vector=NULL, 
                            missing=NULL, newobj=NULL, datasources=NULL, notify.of.progress=FALSE){

  datasources <- .resolveDatasources(datasources)

  # check user has provided the name of the variable to be recoded
  .checkArgProvided(var.name, "the name of the variable to recode")

  # check user has provided the vector specifying the set of values to be replaced
  if(is.null(values2replace.vector)){
    cli_abort(c("{.arg values2replace.vector} must specify the values to replace.", "i" = "For example, {.code c(1, 7)}."))
  }

  # check user has provided the vector specifying the set of values to replace them with
  if(is.null(values2replace.vector)){
    cli_abort(c("{.arg new.values.vector} must specify the new values.", "i" = "For example, {.code c(3, 4)}."))
  }

  # check values2replace.vector and new.values.vector have the same length
  if(length(values2replace.vector) != length(new.values.vector)){
    cli_abort(c("{.arg values2replace.vector} and {.arg new.values.vector} must have the same length and order.", "x" = "{.arg values2replace.vector} has length {length(values2replace.vector)} and {.arg new.values.vector} has length {length(new.values.vector)}."))
  }

  # check no duplicate values in values2replace.vector
  if(length(values2replace.vector) != length(unique(values2replace.vector))){
    cli_abort(c("{.arg values2replace.vector} must not contain duplicate values.", "x" = "Duplicated: {.val {unique(values2replace.vector[duplicated(values2replace.vector)])}}."))
  }
  
  # simple work around for a bug in the format for values2replace.vector
  if(any(is.na(values2replace.vector))){
    cli_abort(c("{.arg values2replace.vector} can't contain {.code NA}.", "i" = "Use {.arg missing} to recode {.code NA} values."))
  }
  
  if(!is.null(values2replace.vector) & !is.null(new.values.vector)){
    values2replace.transmit <- paste0(as.character(values2replace.vector), collapse=",")
    new.values.transmit <- paste0(as.character(new.values.vector), collapse=",")
  }else{
    values2replace.transmit <- NULL
    new.values.transmit <- NULL
  }
    
  newobj <- .resolveNewobjName(newobj, paste0(var.name, "_recoded"))

  calltext <- call("recodeValuesDS", var.name, values2replace.transmit, new.values.transmit, missing)
  DSI::datashield.assign(datasources, newobj, calltext)

}
#ds.recodeValues
