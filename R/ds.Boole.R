#'
#' @title Converts a server-side R object into Boolean indicators
#' @description It compares R objects using the standard set of 
#' Boolean operators (\code{==, !=, >, >=, <, <=}) to create a
#' vector with Boolean indicators that can be of class logical (\code{TRUE/FALSE}) 
#' or numeric (\code{1/0}). 
#' 
#' @details A combination of different Boolean operators using \code{AND} operator
#' can be obtained by multiplying two or more
#' binary/Boolean vectors together. In this way, observations taking the value 1 in every vector
#' will then take the value 1 in the final vector (after multiplication)
#' while all others will take the value 0. Instead the combination using  \code{OR} operator
#' can be obtained by the sum of two or more vectors and applying   
#' \code{ds.Boole} using the operator \code{>= 1}. 
#' 
#' In \code{na.assign} if \code{'NA'} is specified, the missing values 
#' remain as \code{NA}s in the output vector. If \code{'1'} or \code{'0'} is specified the 
#' missing values are converted to 1 or 0 respectively or \code{TRUE}
#' or \code{FALSE} depending on the argument \code{numeric.output}.
#' 
#' 
#' 
#' Server function called: \code{BooleDS}
#' 
#' @param V1 A character string specifying the name of the vector to which the Boolean operator
#' is to be applied. 
#' @param V2 A character string specifying the name of the vector to compare with \code{V1}. 
#' @param Boolean.operator A character string specifying one of six possible Boolean operators:
#' \code{'==', '!=', '>', '>=', '<'} and \code{'<='}. 
#' @param numeric.output logical. If TRUE the output variable should be of class numeric (\code{1/0}).
#' If FALSE the output variable should be of class logical (\code{TRUE/FALSE}). 
#' Default TRUE. 
#' @param na.assign A character string taking values \code{'NA'},\code{'1'} or \code{'0'}.
#' Default \code{'NA'}. For more information see details. 
#' @param newobj 	a character string that provides the name for the output 
#' object that is stored on the data servers. Default \code{boole.newobj}. 
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} 
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#' @return \code{ds.Boole} returns the object specified by the \code{newobj} argument 
#' which is written to the server-side. Also, two validity messages are returned
#' to the client-side indicating the name of the \code{newobj} which 
#' has been created in each data source and if 
#' it is in a valid form.
#' @examples 
#' 
#' \dontrun{
#' 
#'   ## Version 6, for version 5 see the Wiki
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
#'   # Log onto the remote Opal training servers
#'   connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D") 
#'
#'   # Generating Boolean indicators
#'   ds.Boole(V1 = "D$LAB_TSC",
#'            V2 = "D$LAB_TRIG",
#'            Boolean.operator = ">",
#'            numeric.output = TRUE, #Output vector of 0 and 1
#'            na.assign = "NA",      
#'            newobj = "Boole.vec",
#'            datasources = connections[1]) #only the first server is used ("study1")
#'            
#'   ds.Boole(V1 = "D$LAB_TSC",
#'            V2 = "D$LAB_TRIG",
#'            Boolean.operator = "<",
#'            numeric.output = FALSE, #Output vector of TRUE and FALSE 
#'            na.assign = "1", #NA values are converted to TRUE
#'            newobj = "Boole.vec",
#'            datasources = connections[2]) #only the second server is used ("study2") 
#'                       
#'   ds.Boole(V1 = "D$LAB_TSC",
#'            V2 = "D$LAB_TRIG",
#'            Boolean.operator = ">",
#'            numeric.output = TRUE, #Output vector of 0 and 1
#'            na.assign = "0", #NA values are converted to 0      
#'            newobj = "Boole.vec",
#'            datasources = connections) #All servers are used
#'   
#'   # Clear the Datashield R sessions and logout           
#'   datashield.logout(connections)
#' }
#'  
#' @author DataSHIELD Development Team
#' @export
#'
ds.Boole<-function(V1=NULL, V2=NULL, Boolean.operator=NULL, numeric.output=TRUE, na.assign="NA",newobj=NULL, datasources=NULL){

  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }

  # check if user has provided the name of the column or scalar that holds V1
  if(is.null(V1)){
    stop("Please provide the name of the column or scalar that holds V1!", call.=FALSE)
  }

  # check if user has provided the name of a column or scalar holding V2 or has declared a scalar value: eg '3'
  if(is.null(V2)){
    stop("Please provide the name of a column or scalar holding V2 or declare a scalar in character format: eg '3'", call.=FALSE)
  }

  # check if user has provided a Boolean operator in character format: eg '==' or '>=' or '<' or '!='
  if(is.null(Boolean.operator)){
    stop("Please provide a Boolean operator in character format: eg '==' or '>=' or '<' or '!='", call.=FALSE)
  }

  #check if na.assign has legal value
  if(!(na.assign=="NA"||na.assign=="0"||na.assign=="1")){
    stop("Error: na.assign must be a character string taking value 'NA', '0' or '1'- if <na.action> not specified default is 'NA'", call.=FALSE)
  }

  # create a name by default if user did not provide a name for the new variable
  if(is.null(newobj)){
    newobj <- "boole.newobj"
  }

  #convert Boolean operator to numeric

  BO.n<-0
  if(Boolean.operator == "=="){
    BO.n<-1
  }

  if(Boolean.operator == "!="){
    BO.n<-2
  }

  if(Boolean.operator == "<"){
    BO.n<-3
  }

  if(Boolean.operator == "<="){
    BO.n<-4
  }

  if(Boolean.operator == ">"){
    BO.n<-5
  }

  if(Boolean.operator == ">="){
    BO.n<-6
  }

  if(BO.n == 0){
    stop(paste0("An unrecognized Boolean operator, ", Boolean.operator, ", has provide"), call.=FALSE)
  }

  # if no value spcified for output object, then specify a default
  if(is.null(newobj)){
    newobj <- paste0(V1,"_Boole")
  }

  # CALL THE MAIN SERVER SIDE FUNCTION
  calltext <- call("BooleDS", V1, V2, BO.n, na.assign,numeric.output)
  DSI::datashield.assign(datasources, newobj, calltext)

#############################################################################################################
#DataSHIELD CLIENTSIDE MODULE: CHECK KEY DATA OBJECTS SUCCESSFULLY CREATED                                  #
																											#
#SET APPROPRIATE PARAMETERS FOR THIS PARTICULAR FUNCTION                                                 	#
test.obj.name<-newobj																					 	#
																											#
#TRACER																									 	#
#return(test.obj.name)																					 	#
#}                                                                                   					 	#
																											#
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
#ds.Boole
