#' @title Creates a repetitive sequence in the server-side
#' 
#' @description Creates a repetitive sequence by repeating
#' the specified scalar number, vector or list in each data source.
#' @details  All arguments that can denote in a clientside  or
#' a serverside (i.e. \code{x1}, \code{times}, \code{length.out}
#' or \code{each}). 
#' 
#' Server function called: \code{repDS}. 
#' 
#' @param x1 an scalar number, vector or list. 
#' @param times an integer from clientside or a serverside integer
#' or vector. 
#' @param length.out a clientside integer or a serverside integer
#' or vector. 
#' @param each a clientside or serverside integer. 
#' @param source.x1 the source \code{x1} argument. It can be "clientside" or "c" 
#' and serverside or "s".
#' @param source.times see \code{source.x1}
#' @param source.length.out see \code{source.x1}
#' @param source.each see \code{source.x1}
#' @param x1.includes.characters Boolean parameter which specifies if 
#' the \code{x1} is a character. 
#' @param newobj a character string that provides the name for the output object
#' that is stored on the data servers. Default \code{seq.vect}. 
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} objects obtained after login. 
#' If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#' @return \code{ds.rep} returns in the server-side a vector with the specified repetitive sequence.  
#' Also, two validity messages are returned to the client-side
#'  the name of \code{newobj} that has been created 
#' in each data source and if it is in a valid form.
#' @examples 
#' \dontrun{
#' 
#'   ## Version 6, for version 5 see the Wiki
#'   # Connecting to the Opal servers
#' 
#'     require('DSI')
#'     require('DSOpal')
#'     require('dsBaseClient')
#'
#'     builder <- DSI::newDSLoginBuilder()
#'     builder$append(server = "study1", 
#'                    url = "http://192.168.56.100:8080/", 
#'                    user = "administrator", password = "datashield_test&", 
#'                    table = "CNSIM.CNSIM1", driver = "OpalDriver")
#'     builder$append(server = "study2", 
#'                    url = "http://192.168.56.100:8080/", 
#'                    user = "administrator", password = "datashield_test&", 
#'                    table = "CNSIM.CNSIM2", driver = "OpalDriver")
#'     builder$append(server = "study3",
#'                    url = "http://192.168.56.100:8080/", 
#'                    user = "administrator", password = "datashield_test&", 
#'                    table = "CNSIM.CNSIM3", driver = "OpalDriver")
#'     logindata <- builder$build()
#' 
#'   # Log onto the remote Opal training servers
#'     connections <- DSI::datashield.login(logins = logindata, 
#'                                          assign = TRUE, 
#'                                          symbol = "D") 
#' 
#'   # Creating a repetitive sequence  
#'               
#'      ds.rep(x1 = 4,
#'             times = 6,
#'             length.out = NA,
#'             each = 1,
#'             source.x1 = "clientside",
#'             source.times = "c",
#'             source.length.out = NULL,
#'             source.each = "c",
#'             x1.includes.characters = FALSE,
#'             newobj = "rep.seq",
#'             datasources = connections)
#'        
#'      ds.rep(x1 = "lung",
#'             times = 6,
#'             length.out = 7,
#'             each = 1,
#'             source.x1 = "clientside",
#'             source.times = "c",
#'             source.length.out = "c",
#'             source.each = "c",
#'             x1.includes.characters = TRUE,
#'             newobj = "rep.seq",
#'             datasources = connections)
#'
#'   # Clear the Datashield R sessions and logout  
#'   datashield.logout(connections) 
#' } 
#' 
#' @author DataSHIELD Development Team
#' @export
#'
ds.rep<-function(x1=NULL,  times=NA,  length.out=NA, each=1, 
                source.x1='clientside', source.times=NULL,
				source.length.out=NULL,source.each=NULL,
				x1.includes.characters=FALSE,newobj=NULL,datasources=NULL){
  
  # if no connection login details are provided look for 'connection' objects in the environment
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }

  # check if a value has been provided for x1
  if(is.null(x1)){
    return("Error: x1 must have a value which is a character string, a numeric vector on the clientside or a scalar")
  }

  # if no value specified for output object, then specify a default
  if(is.null(newobj))
  {
    newobj <- paste0("seq.vect")
  }

#set up aliases for 'clientside', 'serverside' as 'c' and 's'
if(source.x1=='c')source.x1<-'clientside'
if(source.x1=='s')source.x1<-'serverside'

#must allow for NULL arguments
if(!is.null(source.times))
{
if(source.times=='c')source.times<-'clientside'
if(source.times=='s')source.times<-'serverside'
}

if(!is.null(source.length.out))
{
if(source.length.out=='c')source.length.out<-'clientside'
if(source.length.out=='s')source.length.out<-'serverside'
}

if(!is.null(source.each))
{
if(source.each=='c')source.each<-'clientside'
if(source.each=='s')source.each<-'serverside'
}


 
 
  #########################################################
  #Process 'x1' to make transmittable depending on source.x1
  #########################################################
  #Check that source has been specified
  if(source.x1!="serverside"&&source.x1!="clientside")
  {
  cat("            FAILED: if source.x1 is non-null it must be specified as
  one of the following: 'clientside','serverside','c', or 's'\n\n")
  return('Please respecify')
  }

  #process appropriately for source
  if(source.x1=="clientside")
  {
  x1.transmit<-paste0(as.character(x1),collapse=",")
  }
  
  if(source.x1=="serverside")
  {
  x1.transmit<-x1
  }
 
  ###############################################################
  #Process 'times' to make transmittable depending on source.times
  ###############################################################

 #Check that source has been specified
  if(source.times!="serverside"&&source.times!="clientside"&&!is.null(source.times))
  {
  cat("            FAILED: if source.times is non-null it must be specified as
  one of the following: 'clientside','serverside','c', or 's'\n\n")
  return('Please respecify')
  }


  #process appropriately for source
  if(is.null(source.times))
  {
  times.transmit<-paste0(as.character(times),collapse=",")
  }
  
  else
  {
	if(source.times=="clientside")
	{
	times.transmit<-paste0(as.character(times),collapse=",")
	}
  
	if(source.times=="serverside")
	{
	times.transmit<-times
	}
  }
  
  

  #########################################################################
  #Process 'length.out' to make transmittable depending on source.length.out
  #########################################################################

  #Check that source has been specified
  if(source.length.out!="serverside"&&source.length.out!="clientside"&&!is.null(source.length.out))
  {
  cat("            FAILED: if source.length.out is non-null it must be specified as
  one of the following: 'clientside','serverside','c', or 's'\n\n")
  return('Please respecify')
  }


  #process appropriately for source
  if(is.null(source.length.out))
  {
  length.out.transmit<-paste0(as.character(length.out),collapse=",")
  }
  
  else
  {
	if(source.length.out=="clientside")
	{
	length.out.transmit<-paste0(as.character(length.out),collapse=",")
	}
  
	if(source.length.out=="serverside")
	{
	length.out.transmit<-length.out
	}
  }
  
 
 
  #############################################################
  #Process 'each' to make transmittable depending on source.each
  #############################################################

  
  #Check that source has been specified
  if(source.each!="serverside"&&source.each!="clientside"&&!is.null(source.each))
  {
  cat("            FAILED: if source.each is non-null it must be specified as
  one of the following: 'clientside','serverside','c', or 's'\n\n")
  return('Please respecify')
  }


  #process appropriately for source
  if(is.null(source.each))
  {
  each.transmit<-paste0(as.character(each),collapse=",")
  }
  
  else
  {
	if(source.each=="clientside")
	{
	each.transmit<-paste0(as.character(each),collapse=",")
	}
  
	if(source.each=="serverside")
	{
	each.transmit<-each
	}
  }
  
 
  
# CALL THE MAIN SERVER SIDE FUNCTION

  calltext <- call("repDS", x1.transmit=x1.transmit, times.transmit=times.transmit,
                   length.out.transmit=length.out.transmit, each.transmit=each.transmit,
                   x1.includes.characters=x1.includes.characters,
				   source.x1=source.x1, source.times=source.times,
				   source.length.out=source.length.out, source.each=source.each)

 
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
#ds.rep


