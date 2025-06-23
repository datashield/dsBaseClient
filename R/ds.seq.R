#' @title Generates a sequence in the server-side
#' @description This function generates a sequence for given parameters 
#' on the server-side. 
#' @details This function is similar to a native R function \code{seq()}. 
#' It creates a flexible range of sequence vectors that can then be used to help
#' manage and analyse data. 
#'
#' \strong{Note}: the combinations of arguments that are not allowed
#' for the function \code{seq} in native R are also prohibited in \code{ds.seq}. 
#'  
#' To be specific, \code{FROM.value.char} argument 
#' defines the start of the sequence and \code{BY.value.char} defines how
#' the sequence is incremented (or decremented) at each step. But where the
#' sequence stops can be defined in three different ways:\cr
#' (1) \code{TO.value.char} indicates the terminal value of the sequence. 
#' For example, \code{ds.seq(FROM.value.char = "3", BY.value.char = "2",
#' TO.value.char = "7")}  creates the sequence \code{3,5,7} on the server-side.\cr
#' (2) \code{LENGTH.OUT.value.char} indicates the length of the sequence. 
#' For example, \code{ds.seq(FROM.value.char = "3", BY.value.char = "2", 
#' LENGTH.OUT.value.char = "7")} 
#' creates the sequence \code{3,5,7,9,11,13,15} on the server-side.\cr
#' (3) \code{ALONG.WITH.name} specifies the name of a variable on the server-side,
#' such that the sequence in each study will be equal in length to that variable.
#' For example, \code{ds.seq(FROM.value.char = "3", BY.value.char = "2", 
#' ALONG.WITH.name = "var.x")}
#' creates a sequence such that if \code{var.x} is of length 100 in study 1 the
#' sequence written to study 1 will be \code{3,5,7,...,197,199,201} and if \code{var.x} is
#' of length 4 in study 2, the sequence written to study 2 will be \code{3,5,7,9}.\cr
#' Only one of the three arguments: \code{TO.value.char},
#' \code{LENGTH.OUT.value.char} and \code{ALONG.WITH.name} can be non-null in any one call.
#' 
#' In \code{LENGTH.OUT.value.char} argument if you specify a number with a decimal point but 
#' in character form this result in a sequence \code{length(integer) + 1}.
#' For example, \code{LENGTH.OUT.value.char = "1000.0001"} 
#' generates a sequence of length 1001.
#' 
#' Server function called: \code{seqDS}
#' @param FROM.value.char an integer or a number in character from specifying 
#' the starting value for the sequence. 
#' Default \code{"1"}. 
#' @param TO.value.char an integer or a number in character from specifying 
#' the terminal value for the sequence.
#' Default NULL. 
#' For more information see \strong{Details}. 
#' @param BY.value.char an integer or a number in character from specifying 
#' the value to increment each step in the sequence.
#' Default \code{"1"}. 
#' @param LENGTH.OUT.value.char an integer or a number in character from specifying 
#' the length of the sequence at which point
#' its extension should be stopped.
#' Default NULL. 
#' For more information see \strong{Details}. 
#' @param ALONG.WITH.name a character string specifying the name of a standard vector 
#' to generate a vector of the same length. 
#' For more information see \strong{Details}. 
#' @param newobj a character string that provides the name for the output variable 
#' that is stored on the data servers. Default \code{seq.newobj}.  
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} objects obtained after login. 
#' If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#' @return \code{ds.seq} returns to the server-side the generated sequence. 
#' Also, two validity messages are returned to the client-side 
#' indicating whether the new object has been created in each data source and if so whether
#' it is in a valid form. 
#' @author DataSHIELD Development Team
#' @examples 
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
#'   #Create 3 different sequences
#'   
#'   ds.seq(FROM.value.char = "1",
#'          BY.value.char = "2",
#'          TO.value.char = "7",
#'          newobj = "new.seq1",
#'          datasources = connections)
#'          
#'          
#'   ds.seq(FROM.value.char = "4",
#'          BY.value.char = "3",
#'          LENGTH.OUT.value.char = "10",
#'          newobj = "new.seq2",
#'          datasources = connections)  
#'          
#'   ds.seq(FROM.value.char = "2",
#'          BY.value.char = "5",
#'          ALONG.WITH.name = "D$GENDER",
#'          newobj = "new.seq3",
#'          datasources = connections)                            
#'          
#'   # Clear the Datashield R sessions and logout
#'   datashield.logout(connections) 
#' }
#' @export
ds.seq<-function(FROM.value.char = "1", BY.value.char = "1", TO.value.char=NULL, LENGTH.OUT.value.char = NULL, ALONG.WITH.name=NULL,
                   newobj="newObj", datasources=NULL) {
###datasources
  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }

###FROM.value.char
  # check FROM.value.char is valid
  FROM.valid<-1
  if(!(is.null(FROM.value.char))) {
		if(!is.character(FROM.value.char))FROM.valid<-0
		if(!is.numeric(eval(parse(text=FROM.value.char))))FROM.valid<-0
	}
	if(!FROM.valid){
  return("Error: If FROM.value.char is non.NULL, it must be a real number in inverted commas eg '-3.7' or '0'")
	}

###TO.value.char
  # check TO.value.char is valid
  TO.valid<-1
  if(!(is.null(TO.value.char))) {
		if(!is.character(TO.value.char))TO.valid<-0
		if(!is.numeric(eval(parse(text=TO.value.char))))TO.valid<-0
	}
	if(!TO.valid){
  return("Error: If TO.value.char is non.NULL, it must be a real number in inverted commas eg '-3.7' or '0'")
	}

###BY.value.char
  # check BY.value.char is valid
  BY.valid<-1
  if(!(is.null(BY.value.char))) {
		if(!is.character(BY.value.char))BY.valid<-0
		if(!is.numeric(eval(parse(text=BY.value.char))))BY.valid<-0
	}
	if(!BY.valid){
  return("Error: If FROM.value.char is non.NULL, it must be a real number in inverted commas eg '5' or '-98.7321'")
	}

###LENGTH.OUT.value.char
  # check LENGTH.OUT.value.char is valid
	LENGTH.OUT.valid<-1
  if(!(is.null(LENGTH.OUT.value.char))) {
		if(!is.character(LENGTH.OUT.value.char))LENGTH.OUT.valid<-0
		if(!is.numeric(eval(parse(text=LENGTH.OUT.value.char))))LENGTH.OUT.valid<-0
	}
	if(!LENGTH.OUT.valid){
  return("Error: If LENGTH.OUT.value.char is non.NULL, it must be an integer in inverted commas eg '87187'")
	}


###ALONG.WITH.name
  # check if user has correctly provided the name of a column to hold ALONG.WITH.name
  if(!(is.null(ALONG.WITH.name) || is.character(ALONG.WITH.name))){
    return("Error: If ALONG.WITH.name is non.NULL, it must specify the name of a serverside vector in inverted commas")
	}

###Either LENGTH.OUT.value.char or ALONG.WITH.name must be non-NULL
if(is.null(TO.value.char)&&is.null(LENGTH.OUT.value.char)&&is.null(ALONG.WITH.name)){
    return("Error: Either TO.value.char or LENGTH.OUT.value.char or ALONG.WITH.name must be non-NULL, they cannot both be NULL")
	}



# CALL THE PRIMARY SERVER SIDE FUNCTION
  calltext <- call("seqDS", FROM.value.char,TO.value.char,BY.value.char,LENGTH.OUT.value.char,ALONG.WITH.name)
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
#ds.seq
