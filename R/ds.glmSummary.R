#'
#' @title Summarize a glm object on the serverside
#' @description Summarize a glm object on the serverside to create a 
#' summary_glm object. Also identify and return components of 
#' both the glm object and the summary_glm object
#' that can safely be sent to the clientside without a risk of disclosure 
#' @details Clientside function calling a single assign function (glmSummaryDS.as)
#' and a single aggregate function (glmSummaryDS.as). ds.glmSummary summarises a
#' glm object that has already been created on the serverside by fitting ds.glmSLMA
#' which is precisely the same as the glm object created by fitting a glm
#' using the glm function in native R. Similarly the summary_glm object
#' saved to the serverside is precisely equivalent to the object created using
#' summary(glm object) in R. The glm object produced from a standard
#' call to glm in R has 32 components. Amongst these, all of the following
#' thirteen contain information about every records in the data set and so are
#' disclosive. They are all therefore set to NA and so convey
#' no information when returned to the clientside:
#' 1.residuals, 2.fitted.values, 3.effects, 4.R, 5.qr, 6.linear.predictors,
#' 7.weights, 8.prior.weights, 9.y, 10.model, 11. na.action, 12.x,  13. offset. 
#' In addition the list element "data" which identifies a data.frame
#' that was identified as containing all of the variables required
#' for the model is also disclosive because it doesn't list
#' the name of the data.frame but rather prints it out in full. However,
#' a user can benefit from knowing what source of data were used in creating
#' the glm model and so the element "data" that is returned to the clientside
#' simply lists the names of all of the columns in the originating data.frame.
#' Having removed all disclosive elements of the glm object, ds.glmSummary
#' returns the remaining 19 elements to the clientside.  The object
#' created from a standard call to summary(glm object) in R contains 18
#' list elements. Only two of these are disclosive - na.action and
#' deviance.resid and these are therefore set to NA before ds.glmSummary
#' returns the other 16 to the clientside. Further details of the components
#' of the glm object and summary_glm object can be found under help for
#' glm and summary(glm) in native R. In addition, the elements that ARE returned
#' are listed under "return" below.
#' @param x.name a character string providing the name of a glm object on the
#' serverside that has previously been created e.g. using ds.glmSLMA
#' @param newobj a character string specifying the name of the object to which
#' the summary_glm object representing the output of summary(glm object)
#' in each study is to be written. If no <newobj> argument is specified, the output
#' object on the serverside defaults to "summary_glm.newobj".
#' @param datasources specifies the particular 'connection object(s)' to use.
#' e.g. if you have several data sets in the sources you are working with
#' called opals.a, opals.w2, and connection.xyz, you can choose which of
#' these to work with. The call 'datashield.connections_find()' lists all of
#' the different datasets available and if one of these is called 'default.connections'
#' that will be the dataset used by default if no other dataset is specified. If you
#' wish to change the connections you wish to use by default the call
#' datashield.connections_default('opals.a') will set 'default.connections'
#' to be 'opals.a' and so in the absence of specific instructions to the contrary
#' (e.g. by specifying a particular dataset to be used via the <datasources>
#' argument) all subsequent function calls will be to the datasets held in opals.a.
#' If the <datasources> argument is specified, it should be set without
#' inverted commas: e.g. datasources=opals.a or datasources=default.connections.
#' The <datasources> argument also allows you to apply a function solely to a subset
#' of the studies/sources you are working with. For example, the second source
#' in a set of three, can be specified using a call such as datasources=connection.xyz[2].
#' On the other hand, if you wish to specify solely the first and third sources, the
#' appropriate call will be datasources=connections.xyz[c(1,3)]
#' @return ds.glmSummary writes a new object to the serverside with name given by
#' the newobj argument or if that argument is missing or null it is called "summary_glm.newobj".
#' In addition, ds.glmSummary returns an object containing two lists to the clientside
#' the two lists are named "glm.obj" and "glm.summary.obj" which contain all of the
#' elements of the original glm object and the summary_glm object on the serverside
#' but with all potentially disclosive components set to NA or masked in another way
#' see "details" above. The elements that are returned with a non-NA value in
#' the glm.obj list object are: "coefficients", "rank", "family", "deviance", "aic",
#' "null.deviance", "iter", "df.residual", "df.null", "converged", "boundary",         
#' "call", "formula", "terms", "data", "control", "method", "contrasts", "xlevels".
#' The elements that are returned with a non-NA value in
#' the glm.summary.obj list object are: "call", "terms", "family", "deviance",
#' "aic", "contrasts", "df.residual", "null.deviance", "df.null", "iter",
#' "coefficients", "aliased", "dispersion", "df", "cov.unscaled", "cov.scaled".
#' For further information see help for glm and summary(glm) in native R
#' and for ds.glmSLMA in DataSHIELD.    
#' @author Paul Burton, for DataSHIELD Development Team 17/07/20
#' @export
#'
ds.glmSummary <- function(x.name, newobj=NULL, datasources=NULL) {

  # if no connections are specified look for connection objects in the environment
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }

  # check if a value has been provided for x
  if(is.null(x.name)||!is.character(x.name)){
    stop("Error: x.name must denote a character string naming the glm object on the serverside to be summarised", call.=FALSE)
  }
  
  # check if the input object is defined in all the studies
  isDefined(datasources, x.name)

  # create a name by default if the user did not provide a name for the new object
  if (is.null(newobj)) {
	  newobj <- "summary_glm.newobj"
  }
  
  # PREPARE AND CALL THE ASSIGN FUNCTION TO PREPARE THE summary_glm OBJECT ON THE SERVERSIDE
	calltext1 <- call("glmSummaryDS.as", x.name)
	DSI::datashield.assign(datasources,newobj,calltext1)

  # LOOK BELOW CLIENTSIDE MODULE FOR NEXT BLOCK OF CODE
  # PREPARE AND CALL THE SECOND ASSIGN FUNCTION TO PREPARE AN ABBREVIATED 
  # summary_glm OBJECT ON THE SERVERSIDE THAT CAN SAFELY BE RETURNED TO CLIENT

#############################################################################################################
#DataSHIELD CLIENTSIDE MODULE: CHECK KEY DATA OBJECTS SUCCESSFULLY CREATED                                  #
																											#
#SET APPROPRIATE PARAMETERS FOR THIS PARTICULAR FUNCTION                                                 	#
test.obj.name<-newobj																					 	#
																											#																											#
																											#							
# CALL SEVERSIDE FUNCTION                                                                                	#
calltext <- call("testObjExistsDS", test.obj.name)													 		#
																											#
object.info<-DSI::datashield.aggregate(datasources, calltext)												#
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
#	if(no.errors){																							#
#	cat("\n\nCREATE ASSIGN OBJECT\n")																		#
#																											#
#	validity.check<-paste0("<",test.obj.name, "> appears valid in all sources")							    #
#	print(list(is.object.created=return.message,validity.check=validity.check))						    	#
#	}																										#
																											#
  if(!no.errors){																								#
 	validity.check<-paste0("<",test.obj.name,"> invalid in at least one source. See studyside.messages:")   #
 	print(list(is.object.created=return.message,validity.check=validity.check,					    		#
 	            studyside.messages=studyside.message))			                                            #
 	}																										#
																											#
#END OF CHECK OBJECT CREATED CORECTLY MODULE															 	#
#############################################################################################################


# PREPARE AND CALL THE SECOND SERVERSIDE (AGGREGATE) FUNCTION TO PREPARE AN ABBREVIATED 
# (disclosure controlled) summary_glm OBJECT TO BE BE RETURNED TO CLIENT


	calltext2 <- call("glmSummaryDS.ag", x.name)

	output.list<-DSI::datashield.aggregate(datasources,calltext2)
  
return(output.list)
}
#ds.glmSummary
