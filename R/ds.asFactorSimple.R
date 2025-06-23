#' 
#' @title Converts a numeric vector into a factor
#' @description ds.asFactorSimple calls the assign function asFactorSimpleDS and
#' thereby coerces a numeric or character vector into a factor 
#' @details The function converts the input variable into a factor. Unlike 
#' ds.asFactor and its serverside functions, ds.asFactorSimple does no more than
#' coerce the class of a variable to make it a factor on the serverside in each data source.
#' It does not check for or enforce consistency of factor levels across sources or allow you to
#' force an arbitrary set of levels unless those levels actually exist in the sources.
#' Furthermore, it does not allow you to create an array of
#' binary dummy variables that is equivalent to a factor. If you need to do any
#' of these things you will have to use the ds.asFactor function.
#' @param input.var.name a character string which provides 
#' the name of the variable to be converted to a factor. 
#' @param newobj.name a character string that provides the name for the output variable 
#' that is stored on the data servers. Default \code{asfactor.newobj}. 
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} objects obtained after login. 
#' If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#' @return an output vector of class factor to the serverside. In addition, returns a validity 
#' message with the name of the created object on the client-side and if creation fails an
#' error message which can be viewed using datashield.errors().  
#' @author DataSHIELD Development Team
#' @export
#'
ds.asFactorSimple <- function(input.var.name=NULL, newobj.name=NULL, datasources=NULL){

  # look for DS connections
  if(is.null(datasources)){
     datasources <- datashield.connections_find()
  }

  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }

  # check if user has provided the name of the column that holds the input variable
  if(is.null(input.var.name)){
    stop("Please provide the name of the variable that is to be converted to a factor e.g. 'varname'", call.=FALSE)
  }

  # check if user has provided the name of the input variable in a correct character format
  if(!is.character(input.var.name)){
    stop("Please provide the name of the variable that is to be converted to a factor in character format e.g. 'varname'", call.=FALSE)
  }

  # if no output variable specified then provide a default name
  if(is.null(newobj.name)){
    newobj.name <- "asfactor.newobj"
  }


#Call the only serverside function required for this simple version of asFactor
  calltext0 <- call("asFactorSimpleDS", input.var.name)
  DSI::datashield.assign(datasources, newobj.name, calltext0)

##########################################################################################################
#MODULE 5: CHECK KEY DATA OBJECTS SUCCESSFULLY CREATED                                                   #
																										 #
#SET APPROPRIATE PARAMETERS FOR THIS PARTICULAR FUNCTION                                                 #
test.obj.name<-newobj.name                                                                               #
                                                                                                         #
# CALL SEVERSIDE FUNCTION                                                                                #
calltext <- call("testObjExistsDS", test.obj.name)													 #
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
    paste0("Data object <", test.obj.name, "> correctly created in all specified data sources")		 	 #
																										 #
	return(list(return.message=return.message))						 #
																										 #
	}else{																								 #
																										 #
    return.message.1<-																					 #
	paste0("Error: A valid data object <", test.obj.name, "> does NOT exist in ALL specified data sources")#
																										 #
	return.message.2<-																					 #
	paste0("It is either ABSENT and/or has no valid content/class,see return.info above")				 #
																										 #
	return.message<-list(return.message.1,return.message.2)												 #
																										 #
	return.info<-object.info																			 #
																										 #
return(list(return.info=return.info,return.message=return.message))	 #
																										 #
	}																									 #
#END OF MODULE 5																						 #
##########################################################################################################


}
#ds.asFactorSimple
