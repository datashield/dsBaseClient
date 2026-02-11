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
#' Also, two validity messages are returned to the client-side 
#' indicating whether the new object has been created in each data source and if so whether
#' it is in a valid form. 
#' @author DataSHIELD Development Team
#' @examples
#' \dontrun{
#' 
#'   ## Version 6, for version 5 see the Wiki
#'   
#'   # connecting to the Opal servers
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

  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }

  # check user has provided the name of the variable to be recoded
  if(is.null(var.name)){
    stop("Please provide the name of the variable to be recoded: eg 'xxx'", call.=FALSE)
  }
  
  # check if the input object is defined in all the studies
  isDefined(datasources, var.name)
  
  # check user has provided the vector specifying the set of values to be replaced
  if(is.null(values2replace.vector)){
    stop("Please provide a vector in the 'values2replace.vector' argument specifying
         the values to be replaced eg c(1,7)", call.=FALSE)
  }

  # check user has provided the vector specifying the set of values to replace them with
  if(is.null(values2replace.vector)){
    stop("Please provide a vector specifying the new values to be set eg c(3,4)", call.=FALSE)
  }

  # check values2replace.vector and new.values.vector have the same length
  if(length(values2replace.vector) != length(new.values.vector)){
    stop("Please ensure that values2replace.vector and new.values.vector have same length and are in the same order", call.=FALSE)
  }

  # check no duplicate values in values2replace.vector
  if(length(values2replace.vector) != length(unique(values2replace.vector))){
    stop("No value may appear more than once in the values2replace.vector", call.=FALSE)
  }
  
  # simple work around for a bug in the format for values2replace.vector
  if(any(is.na(values2replace.vector))){
    stop("To recode NAs you need to use the 'missing' argument", call.=FALSE)
  }
  
  if(!is.null(values2replace.vector) & !is.null(new.values.vector)){
    values2replace.transmit <- paste0(as.character(values2replace.vector), collapse=",")
    new.values.transmit <- paste0(as.character(new.values.vector), collapse=",")
  }else{
    values2replace.transmit <- NULL
    new.values.transmit <- NULL
  }
    
  if(is.null(newobj)){
    newobj <- paste0(var.name, "_recoded")
  }

  calltext <- call("recodeValuesDS", var.name, values2replace.transmit, new.values.transmit, missing)
  DSI::datashield.assign(datasources, newobj, calltext)

#############################################################################################################
#DataSHIELD CLIENTSIDE MODULE: CHECK KEY DATA OBJECTS SUCCESSFULLY CREATED                                  #
																											#
#SET APPROPRIATE PARAMETERS FOR THIS PARTICULAR FUNCTION                                                 	#
test.obj.name<-newobj																					 	#
																											#																											#
																											#
# CALL SEVERSIDE FUNCTION                                                                                	#
calltext <- call("testObjExistsDS", test.obj.name)													 	#
																											#
object.info<-DSI::datashield.aggregate(datasources, calltext)												 	#
																											#
# CHECK IN EACH SOURCE WHETHER OBJECT NAME EXISTS														 	#
# AND WHETHER OBJECT PHYSICALLY EXISTS WITH A NON-NULL CLASS											 	#
num.datasources<-length(object.info)																	 	#
																											#
																											#
obj.name.exists.in.all.sources<-TRUE																	 	#
obj.non.null.in.all.sources<-TRUE																		 	#
																											#
for(j in 1:num.datasources){																			 	#
	if(!object.info[[j]]$test.obj.exists){																 	#
		obj.name.exists.in.all.sources<-FALSE															 	#
		}																								 	#
	if(is.null(object.info[[j]]$test.obj.class) || ("ABSENT" %in% object.info[[j]]$test.obj.class)){														 	#
		obj.non.null.in.all.sources<-FALSE																 	#
		}																								 	#
	}																									 	#
																											#
if(obj.name.exists.in.all.sources && obj.non.null.in.all.sources){										 	#
																											#
	return.message<-																					 	#
    paste0("A data object <", test.obj.name, "> has been created in all specified data sources")		 	#
																											#
																											#
	}else{																								 	#
																											#
    return.message.1<-																					 	#
	paste0("Error: A valid data object <", test.obj.name, "> does NOT exist in ALL specified data sources")	#
																											#
	return.message.2<-																					 	#
	paste0("It is either ABSENT and/or has no valid content/class,see return.info above")				 	#
																											#
	return.message.3<-																					 	#
	paste0("Please use ds.ls() to identify where missing")												 	#
																											#
																											#
	return.message<-list(return.message.1,return.message.2,return.message.3)							 	#
																											#
	}																										#
																											#
	calltext <- call("messageDS", test.obj.name)															#
    studyside.message<-DSI::datashield.aggregate(datasources, calltext)											#
																											#
	no.errors<-TRUE																							#
	for(nd in 1:num.datasources){																			#
		if(studyside.message[[nd]]!="ALL OK: there are no studysideMessage(s) on this datasource"){			#
		no.errors<-FALSE																					#
		}																									#
	}																										#
																											#
																											#
	if(no.errors){																							#
	validity.check<-paste0("<",test.obj.name, "> appears valid in all sources")							    #
	return(list(is.object.created=return.message,validity.check=validity.check))						    #
	}																										#
																											#
if(!no.errors){																								#
	validity.check<-paste0("<",test.obj.name,"> invalid in at least one source. See studyside.messages:")   #
	return(list(is.object.created=return.message,validity.check=validity.check,					    		#
	            studyside.messages=studyside.message))			                                            #
	}																										#
																											#
#END OF CHECK OBJECT CREATED CORECTLY MODULE															 	#
#############################################################################################################

}
#ds.recodeValues
