#' @title Reshapes server-side grouped data 
#' @description Reshapes a data frame containing longitudinal or
#' otherwise grouped data from 'wide' to 'long' format or vice-versa. 
#' @details This function is based on the native R function \code{reshape}.
#' It reshapes a data frame containing longitudinal or otherwise grouped data
#' between 'wide' format with repeated
#' measurements in separate columns of the same record and 'long' format with the repeated
#' measurements in separate records. The reshaping can be in either direction. 
#' Server function called: \code{reShapeDS}
#' @param data.name a character string specifying the name of the data frame to be reshaped.
#' @param varying names of sets of variables in the wide format that correspond to single
#' variables in 'long' format. 
#' @param v.names the names of variables in the 'long' format that correspond to multiple variables
#' in the 'wide' format. 
#' @param timevar.name the variable in 'long' format that differentiates multiple
#' records from the same group or individual. 
#' If more than one record matches, the first will be taken. 
#' @param idvar.name  names of one or more variables in 'long' format that identify multiple
#' records from the same group/individual. These variables may also be present in 'wide' format.
#' @param drop a vector of names of variables to drop before reshaping. This can simplify the
#' resultant output.
#' @param direction a character string that partially matched to either 'wide' to reshape from
#' 'long' to 'wide' format, or 'long' to reshape from 'wide' to 'long' format.
#' @param sep a character vector of length 1, indicating a separating character in the variable
#' names in the 'wide' format. This is used for creating good \code{v.names} and times arguments based
#' on the names in the \code{varying} argument. This is also used to create variable names
#' when reshaping
#' to 'wide' format. 
#' @param newobj a character string that provides the name for the output object
#' that is stored on the data servers. 
#' Default \code{reshape.newobj}.
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} 
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#' @return \code{ds.reShape} returns to the server-side  a reshaped data frame 
#' converted from 'long' to 'wide' format or from 'wide' to long' format. 
#' Also, two validity messages are returned to the client-side
#' indicating whether the new object  has been created in each data source and if so whether
#' it is in a valid form. 
#' @author DataSHIELD Development Team
#' @examples
#' \dontrun{
#' 
#'  ## Version 6, for version 5 see Wiki
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
#'                  table = "SURVIVAL.EXPAND_NO_MISSING1", driver = "OpalDriver")
#'   builder$append(server = "study2", 
#'                  url = "http://192.168.56.100:8080/", 
#'                  user = "administrator", password = "datashield_test&", 
#'                  table = "SURVIVAL.EXPAND_NO_MISSING2", driver = "OpalDriver")
#'   builder$append(server = "study3",
#'                  url = "http://192.168.56.100:8080/", 
#'                  user = "administrator", password = "datashield_test&", 
#'                  table = "SURVIVAL.EXPAND_NO_MISSING3", driver = "OpalDriver")
#'   logindata <- builder$build()
#'   
#'   # Log onto the remote Opal training servers
#'   connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D") 
#'   
#'   #Reshape server-side grouped data
#'   
#'   ds.reShape(data.name = "D", 
#'              v.names = "age.60", 
#'              timevar.name = "time.id",
#'              idvar.name = "id",
#'              direction = "wide",
#'              newobj = "reshape1_obj",
#'              datasources = connections)
#'   
#'   # Clear the Datashield R sessions and logout
#'   datashield.logout(connections) 
#'}
#'
#' @export
ds.reShape <- function(data.name=NULL, varying=NULL, v.names=NULL, timevar.name="time", idvar.name="id",
                            drop=NULL, direction=NULL, sep=".", newobj="newObject", datasources=NULL){

  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }

  if(is.null(data.name)){
    stop("Please provide the name of the list that holds the input vectors!", call.=FALSE)
  }

  if (!is.character(sep) || (nchar(sep) != 1)){
    stop("'sep' must be a character string", call.=FALSE)
  }

  if(!is.null(varying)){
    varying.transmit <- paste(varying,collapse=",")
  }else{
    varying.transmit <- NULL
  }

  if(!is.null(v.names)){
    v.names.transmit <- paste(v.names,collapse=",")
  }else{
    v.names.transmit <- NULL
  }

  if(!is.null(drop)){
    drop.transmit <- paste(drop,collapse=",")
  }else{
    drop.transmit <- NULL
  }

  ##############################
  # call the server side function
  calltext <- call("reShapeDS", data.name, varying.transmit, v.names.transmit, timevar.name, idvar.name, drop.transmit, direction, sep)
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
#ds.reShape
