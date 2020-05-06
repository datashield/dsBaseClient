#' @title ds.unList calling assign function unListDS
#' @description this function is based on the native R function {unlist}
#' which coerces an object of list class back to the class it was when
#' it was coerced into a list
#' @details See details of the native R function {unlist}.
#' This function represents a substantive restructuring of an earlier version
#' created by Amadou Gaye.
#' When an object is coerced to a list, depending
#' on the class of the original object some information may be lost. Thus,
#' for example, when a data.frame is coerced to a list information that
#' underpins the structure of the data.frame is lost and when it is
#' subject to the function {ds.unList.o} it is returned to a simpler
#' class than data.frame eg 'numeric' (basically a numeric vector
#' containing all of the original data in all variables in the data.frame
#' but with no structure). If you wish to reconstruct the original
#' data.frame you therefore need to specify this structure again e.g.
#' the column names etc
#' @param x.name the name of the input object to be unlisted.
#' It must be specified in inverted commas e.g. x.name="input.object.name"
#' @param newobj the name of the new output variable. If this argument is set
#' to NULL, the name of the new variable is defaulted to unlist.newobj
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login. If the <datasources>
#' the default set of connections will be used: see \link{datashield.connections_default}.
#' @return the object specified by the <newobj> argument (or by default <x.name>.unlist
#' if the <newobj> argument is NULL) which is written to the serverside.
#' As well as writing the output object as <newobj>
#' on the serverside, two validity messages are returned
#' indicating whether <newobj> has been created in each data source and if so whether
#' it is in a valid form. If its form is not valid in at least one study - e.g. because
#' a disclosure trap was tripped and creation of the full output object was blocked -
#' {ds.seq()} also returns any studysideMessages that can explain the error in creating
#' the full output object. As well as appearing on the screen at run time,if you wish to
#' see the relevant studysideMessages at a later date you can use the {ds.message}
#' function. If you type ds.message("<newobj>") it will print out the relevant
#' studysideMessage from any datasource in which there was an error in creating <newobj>
#' and a studysideMessage was saved. Because the outcome object from ds.unList is
#' typically a list object with no names, if there are no errors in creating it
#' the message returned from ds.message("<newobj>") in each study will read
#' "Outcome object is a list without names. So a studysideMessage may be hidden.
#' Please check output is OK". This suggests that - in the case of this specific
#' function - one should check as far as one can
#' the nature of the output from a call to ds.unList - e.g. ds.class, ds.length etc
#' @author Amadou Gaye (2016), Paul Burton (19/09/2019) for DataSHIELD Development Team
#' @export
ds.unList <- function(x.name=NULL, newobj=NULL, datasources=NULL){

  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  if(is.null(x.name)){
    stop("Please provide the name of the input vector!", call.=FALSE)
  }


  # create a name by default if user did not provide a name for the new variable
  if(is.null(newobj)){
    newobj <- "unlist.newobj"
  }

     # call the server side function
  calltext <- call("unListDS", x.name)
  DSI::datashield.assign(datasources, newobj, calltext)


#############################################################################################################
#DataSHIELD CLIENTSIDE MODULE: CHECK KEY DATA OBJECTS SUCCESSFULLY CREATED                                  #
																											#
#SET APPROPRIATE PARAMETERS FOR THIS PARTICULAR FUNCTION                                                 	#
test.obj.name<-newobj																					 	#																											#																											#
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
#ds.unList

