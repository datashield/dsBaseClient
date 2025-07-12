#'
#' @title Calculates a new object in the server-side  
#' @description This function defines a new object in the server-side
#'  via an allowed function or an arithmetic expression. 
#' 
#' \code{ds.make} function is equivalent to \code{ds.assign}, but runs slightly faster.

#' @details 
#' If the new object is created successfully, the function will verify its
#' existence on the required servers. Please note there are certain modes of failure
#' where it is reported that the object has been created but it is not there. This
#' reflects a failure in the processing of some sort and warrants further exploration of the
#' details of the call to \code{ds.make} and the variables/objects which it invokes.
#' 
#' TROUBLESHOOTING: please note we have recently identified an error that makes \code{ds.make}
#' fail and DataSHIELD crash.  
#' 
#' The error arises from a call such as 
#' \code{ds.make(toAssign = '5.3 + beta*xvar', newobj = 'predvals')}. 
#' This is a typical call you
#' may make to get the predicted values from a simple linear regression model where
#' a \code{y} variable is regressed against an \code{x} variable (\code{xvar}) 
#' where the estimated regression intercept is \code{5.3} 
#' and \code{beta} is the estimated regression slope. 
#' 
#' This call appears to
#' fail because in interpreting the arithmetic function which is its first argument
#' it first encounters the (length 1) scalar \code{5.3} 
#' and when it then encounters the \code{xvar} vector
#' which has more than one element it fails - apparently because it does not recognise
#' that you need to replicate the \code{5.3} value the appropriate number of times
#' to create a vector
#' of length equal to \code{xvar} with each value equal to \code{5.3}. 
#' 
#' There are two work-around solutions here: 
#' 
#' (1) explicitly create a vector of appropriate length with each
#' value equal to \code{5.3}. To do this there is a useful trick. First identify
#' a convenient numeric variable with no missing values (typically a numeric
#' individual ID) let us call it \code{indID} equal in length to \code{xvar} (\code{xvar} may include NAs
#' but that doesn't matter provided \code{indID} is the same total length). Then issue the call
#' \code{ds.make(toAssign = 'indID-indID+1',newobj = 'ONES')}. This creates a vector of ones (called \code{ONES})
#' in each source equal in length to the \code{indID} vector in that source. Then issue
#' the second call \code{ds.make(toAssign = 'ONES*5.3',newobj = 'vect5.3')} which creates the required
#' vector of length equal to \code{xvar} with all elements \code{5.3}. Finally, you can
#' now issue a modified call to reflect what was originally needed:
#' \code{ds.make(toAssign = 'vect5.3+beta*xvar', 'predvals')}. 
#' 
#' (2) Alternatively, if you simply swap the original call around: 
#' \code{ds.make(toAssign = '(beta*xvar)+5.3', newobj = 'predvals')}
#' the error seems also to be circumvented. This is presumably because the first element
#' of the arithmetic function is of length equal to \code{xvar} and it then knows to
#' replicate the \code{5.3} that many times in the second part of the expression.
#' 
#' The second work-around is easier, but it is worth knowing about the
#' first trick because creating a vector of ones of equal length to another vector
#' can be useful in other settings. Equally the call:
#' \code{ds.make(toAssign = 'indID-indID',newobj = 'ZEROS')} 
#' to create a vector of zeros of that same length may also be useful.
#' 
#' Server function : \code{messageDS}
#' 
#' The \code{ds.make} function is a wrapper for the DSI package function \code{datashield.assign}
#' @param toAssign a character string specifying the function  or the arithmetic expression.
#' @param newobj a character string that provides the name for the output 
#' variable that is stored on the data servers. Default \code{make.newobj}. 
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} 
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#' @return \code{ds.make} returns the new object which is written to the
#' server-side. Also a validity message is returned to the client-side indicating whether the new object has been correctly
#' created at each source.
#' @author DataSHIELD Development Team
#' @export
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
#'   connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D") 
#'   
#' ##Example 1: arithmetic operators 
#' 
#' ds.make(toAssign = "D$age.60 + D$bmi.26", 
#'         newobj = "exprs1", 
#'         datasources = connections)
#'         
#' ds.make(toAssign = "D$noise.56 + D$pm10.16",
#'         newobj = "exprs2", 
#'         datasources = connections)
#'         
#' ds.make(toAssign = "(exprs1*exprs2)/3.2",
#'         newobj = "result.example1", 
#'         datasources = connections)
#'
#' ##Example 2: miscellaneous operators within functions
#' 
#' ds.make(toAssign = "(D$female)^2",
#'         newobj = "female2",
#'        datasources = connections)
#'        
#' ds.make(toAssign = "(2*D$female)+(D$log.surv)-(female2*2)",
#'         newobj = "output.test.1",
#'         datasources = connections)
#'         
#' ds.make(toAssign = "exp(output.test.1)",
#'         newobj = "output.test",
#'        datasources = connections)
#'        
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(connections)
#' }
#'
ds.make<-function(toAssign=NULL, newobj=NULL, datasources=NULL){

  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }

  if(is.null(toAssign)){
    stop("Please give the name of object to assign or an expression to evaluate and assign.!\n", call.=FALSE)
  }
  
  # create a name by default if user did not provide a name for the new variable
  if(is.null(newobj)){
    newobj <- "make.newobj"
  }

  # now do the business
  DSI::datashield.assign(datasources, newobj, as.symbol(toAssign))

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

# ds.make
