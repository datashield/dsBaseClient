#' @title ds.completeCases calling assign function completeCasesDS
#' @description Identifies and strips out all rows of a data.frame,
#' matrix or vector that contain NAs.
#' @details In the case of a data.frame or matrix, {ds.completeCases} identifies
#' all rows containing one or more NAs and deletes those
#' rows altogether. Any one variable with NA in a given row will lead
#' to deletion of the whole row. In the case of a vector, {ds.completeCases}
#' acts in an equivalent manner but there is no equivalent to a 'row'
#' and so it simply strips out all observations recorded as NA.
#' {ds.completeCASES} is analogous to the {complete.cases} function
#' in native R. Limited additional information can therefore be found
#' under help("complete.cases") in native R.
#' @param x1 This argument determines the input data.frame, matrix or vector
#' from which rows with NAs are to be stripped. x1 is specified via
#' a character string (in inverted commas) denoting the name of the input
#' object
#' @param newobj A character string specifying the name of the output object
#' to be written to the serverside which will be in the form of a modified
#' version of the input data object (data.frame, matrix or vector). The only
#' change is that any rows containing at least one NA will have been stripped.
#' If no <newobj> argument is specified, the output object name defaults to
#' <x1>_complete.cases.
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login. If the <datasources>
#' the default set of connections will be used: see \link{datashield.connections_default}.
#' If the <datasources> is to be specified, it should be set without
#' inverted commas: e.g. datasources=connections.em or datasources=default.connections. If you wish to
#' apply the function solely to e.g. the second connection server in a set of three,
#' the argument can be specified as: e.g. datasources=connections.em[2].
#' If you wish to specify the first and third connection servers in a set you specify:
#' e.g. datasources=connections.em[c(1,3)]. #' @return a modified data.frame, matrix or vector from which
#' all rows containing at least one NA have been deleted. This
#' modified object is written to the serverside in each source.
#' In addition, two validity messages are returned
#' indicating whether <newobj> has been created in each data source and if so whether
#' it is in a valid form. If its form is not valid in at least one study - e.g. because
#' a disclosure trap was tripped and creation of the full output object was blocked -
#' ds.completeCases also returns any studysideMessages that can help
#' explain the error in creating
#' the full output object. As well as appearing on the screen at run time,if you wish to
#' see the relevant studysideMessages at a later date you can use the {ds.message}
#' function. If you type ds.message("newobj") it will print out the relevant
#' studysideMessage from any datasource in which there was an error in creating <newobj>
#' and a studysideMessage was saved. If there was no error and <newobj> was created
#' without problems no studysideMessage will have been saved and ds.message("newobj")
#' will return the message: "ALL OK: there are no studysideMessage(s) on this datasource".
#' @author Paul Burton for DataSHIELD Development Team, 17/10/2019
#' @export
ds.completeCases<-function(x1=NULL, newobj=NULL,datasources=NULL){
  
  # if no connection login details are provided look for 'connection' objects in the environment
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

 # check if a value has been provided for x1
  if(is.null(x1)){
    return("Error: x1 must have a value which is a character string naming a serverside data.frame, matrix or vector")
  }

 #rename target object for transfer (not strictly necessary as string will pass parser anyway)
 #but maintains consistency with other functions
 
  x1.transmit<-x1

  # if no value specified for output object, then specify a default
  if(is.null(newobj))
  {
    newobj <- paste0(x1,"_complete.cases")
  }

  
# CALL THE MAIN SERVER SIDE FUNCTION

  calltext <- call("completeCasesDS", x1.transmit)


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
	if(is.null(object.info[[j]]$test.obj.class) || object.info[[j]]$test.obj.class=="ABSENT"){														 	#
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
#ds.completeCases


