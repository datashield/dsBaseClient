#'
#' @title Checks if an R object exists on the server-side
#' @description This function checks that a specified data object exists or has been correctly created on a
#' specified set of data servers. 
#' @details Close copies of the code in this function
#' are embedded into other functions that create an object and you then wish to test whether it has successfully
#' been created e.g. \code{ds.make} or \code{ds.asFactor}. 
#' 
#' Server function called: \code{testObjExistsDS}
#' @param test.obj.name a character string specifying the name of the object to search. 
#' @param datasources  a list of \code{\link[DSI]{DSConnection-class}} 
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#' @return \code{ds.testObjExists} returns a list of messages specifying that the object exists
#' on the server-side.
#' If the specified object does not exist in at least one 
#' of the specified data sources or it exists but is of
#' class NULL, the function returns an error message specifying that
#' the object does not exist in all data sources. 
#' @examples 
#' \dontrun{
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
#'   #Check if D object exists on the server-side
#'   
#'   ds.testObjExists(test.obj.name = "D",
#'                    datasources = connections)
#'  
#'   # Clear the Datashield R sessions and logout                 
#'   datashield.logout(connections) 
#'   
#' }   
#' @author DataSHIELD Development Team
#' @export
ds.testObjExists <- function(test.obj.name=NULL, datasources=NULL){

  # look for DS connections
  if(is.null(datasources)){
     datasources <- datashield.connections_find()
  }

  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }

  # if not character send error message requesting valic object name
  if(!is.character(test.obj.name)){
    return.message <- "Error: please provide the name of an object on the data servers as a character string in inverted commas"
    return(return.message=return.message)
  }

##########################################################################################################
#MODULE 5: CHECK KEY DATA OBJECTS EXIST IN EACH SOURCE                                                   #
                                                                                                         #
#SET APPROPRIATE PARAMETERS FOR THIS PARTICULAR FUNCTION                                                 #
test.obj.name<-test.obj.name                                                                             #
                                                                                                         #
# CALL SEVERSIDE FUNCTION                                                                                #
calltext <- call("testObjExistsDS", test.obj.name)													 #
																										 #
object.info<-DSI::datashield.aggregate(datasources, calltext)												 #
																										 #
# CHECK IN EACH SOURCE WHETHER OBJECT NAME EXISTS														 #
# AND WHETHER OBJECT PHYSICALLY EXISTS WITH A NON-NULL CLASS											 #
num.datasources<-length(object.info)																	 #
																										 #
																										 #
obj.name.exists.in.all.sources<-TRUE																	 #
obj.non.null.in.all.sources<-TRUE																		 #
																										 #
for(j in 1:num.datasources){																			 #
	if(!object.info[[j]]$test.obj.exists){																 #
		obj.name.exists.in.all.sources<-FALSE															 #
		}																								 #
	if(is.null(object.info[[j]]$test.obj.class) || ("ABSENT" %in% object.info[[j]]$test.obj.class)){														 #
		obj.non.null.in.all.sources<-FALSE																 #
		}																								 #
	}																									 #
																										 #
if(obj.name.exists.in.all.sources && obj.non.null.in.all.sources){										 #
																										 #
	return.message<-																					 #
    paste0("A valid copy of data object <", test.obj.name, "> exists in all specified data sources")     #
																										 #
	return(list(return.message=return.message))															 #
																										 #
	}else{																								 #
																										 #
    return.message.1<-																					 #
	paste0("Error: A valid data object <", test.obj.name, "> does NOT exist in ALL specified data sources")#
																										 #
	return.message.2<-																					 #
	paste0("It is either ABSENT and/or has no valid content/class, for details see return.info above")	 #
																										 #
	return.message<-list(return.message.1,return.message.2)												 #
																										 #
	return.info<-object.info																			 #
																										 #
	return(list(return.info=return.info,return.message=return.message))									 #
																										 #
	}																									 #
#END OF MODULE 5																						 #
##########################################################################################################

  # check in each source whether object name exists
  # and whether object physically exists with a non-null class
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
    return.message <- paste0("Data object ", test.obj.name, " exists in all sources")
	return(list(return.message=return.message))
  }else{
	return.message.1 <- paste0("Error: A valid data object ", test.obj.name, " does NOT exist in all sources")
    return.message.2 <- paste0("It is either ABSENT and/or has no valid content/class,see return.info above")
	return.message <- list(return.message.1,return.message.2)
	return.info <- object.info
    return(list(return.info=return.info,return.message=return.message))
  }

}
#ds.testObjExists
