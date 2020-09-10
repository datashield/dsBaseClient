#' @title Recodes server-side variable values
#' @description This function takes specified values of elements in a vector and converts
#' them to a matched set of alternative specified values.
#' @details This function recodes individual values with new individual values. This can
#' apply to numeric values, character values and NAs. 
#' 
#' One particular use of
#' \code{ds.recodeValues} is to convert NAs to an explicit value or vice-versa.
#' 
#' The argument \code{force.output.format} can be specified in 3 ways: \cr
#' (1) \code{force.output.format = "numeric"} the output 
#' vector will be of type numeric and any non-numeric values in 
#' \code{new.values.vector} will appear as \code{NaN} in the recoded vector.\cr
#' (2) \code{force.output.format = "character"}  all values
#' in the output vector will be in character format. \cr
#' (3) \code{force.output.format = "no"} 
#' if the vector identified by the \code{values2replace.vector} argument is itself
#' numeric and if all values in the \code{new.values.vector} are numeric,
#' the recoded output vector will also be numeric. Otherwise, it will be coerced
#' to character format.
#' 
#' Server functions called: \code{recodeValuesDS1} and \code{recodeValuesDS2}
#' @param var.name a character string providing the name of the variable to be recoded. 
#' @param values2replace.vector a numeric or character vector specifying the values
#' in the variable \code{var.name} to be replaced. 
#' @param new.values.vector a numeric or character vector specifying the new values. 
#' @param force.output.format a character string specifying the format of the output variable. 
#' This can be set as \code{"numeric"}, \code{"character"} or \code{"no"}. Default \code{"no"}.  
#' For more information see \strong{Details}. 
#' @param newobj a character string that provides the name for the output object
#' that is stored on the data servers.
#' Default \code{recodevalues.newobj}. 
#' @param datasources a list of \code{\link{DSConnection-class}} 
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link{datashield.connections_default}}.
#' @param notify.of.progress logical. If TRUE console output should be produced to indicate
#' progress. Default FALSE.
#' @return \code{ds.recodeValues} returns to the server-side the new variable with the recode values. 
#' Also, two validity messages are returned to the client-side 
#' indicating whether the new object  has been created in each data source and if so whether
#' it is in a valid form. 
#' @author DataSHIELD Development Team
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
#'   #Create a vector in the server-side
#'   
#'   ds.assign(toAssign = "D$LAB_TSC", 
#'             newobj = "ss.vector", 
#'             datasources = connections)
#'   
#'   # Recode the values of the vector
#'   
#'   ds.recodeValues(var.name = "ss.vector",
#'                   values2replace.vector = c(0,NA),
#'                   new.values.vector = c(0,0),
#'                   force.output.format = "numeric",
#'                   newobj = "recode.vector",
#'                   datasources = connections,
#'                   notify.of.progress = FALSE)
#'                  
#'   # Clear the Datashield R sessions and logout                 
#'   datashield.logout(connections) 
#'   
#' }   
#' @export

ds.recodeValues<-function(var.name=NULL, values2replace.vector=NULL, new.values.vector=NULL, force.output.format="no", newobj=NULL, datasources=NULL, notify.of.progress=FALSE){

   # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }


    # check user has provided the name of the variable to be recoded
  if(is.null(var.name)){
    stop("Please provide the name of the variable to be recoded: eg 'xxx'", call.=FALSE)
  }

    # check user has provided the vector specifying the set of values to be replaced
  if(is.null(values2replace.vector)){
    stop("Please provide a vector specifying the values to be replaced eg c(1,7,NA)", call.=FALSE)
  }

    # check user has provided the vector specifying the set of values to replace them with
  if(is.null(new.values.vector)){
    stop("Please provide a vector specifying the new values to be set eg c(3,NA,4)", call.=FALSE)
  }

    # check values2replace.vector and new.values.vector have the same length
  if(length(values2replace.vector)!=length(new.values.vector)){
    stop("Please ensure that values2replace.vector and new.values.vector have same length and are in the same order", call.=FALSE)
  }


    # check no duplicate values in values2replace.vector
  if(length(values2replace.vector)!=length(unique(values2replace.vector))){
    stop("No value may appear more than once in the values2replace.vector", call.=FALSE)
  }
    # simple work around for a bug in the format for values2replace.vector

if(length(values2replace.vector)==1&&is.na(values2replace.vector)){
stop("The <values2replace.vector> consists solely of one element which is NA. Please see details
in the help information for ds.recodeValues to find an easy work around that circumvents
the coding restriction that prohibits this particular way of specifying this recoding request")
  }

#DETERMINE WHETHER new.values.vector CONTAINS NON-NUMERIC ELEMENTS (IF SO CAN ONLY GET NUMERIC OUTPUT
#BY force.output.format="numeric" AND NON-NUMERICS WILL THEN BE SET AS NaN)

	#is new.values.vector all NA?
	nvv.all.NA<-(sum(is.na(new.values.vector))==length(new.values.vector))
	nvv.numeric<-is.numeric(new.values.vector)

 numeric.output.format.possible<-(nvv.all.NA||nvv.numeric)

    #is values2replace.vector numeric?

	v2r.numeric<-is.numeric(values2replace.vector)


  values2replace.transmit<-paste0(as.character(values2replace.vector),collapse=",")

  new.values.transmit<-paste0(as.character(new.values.vector),collapse=",")


	 if(is.null(newobj)){newobj<-paste0(var.name,"_recoded")}


    calltext1 <- call("recodeValuesDS1", var.name, values2replace.transmit, new.values.transmit)
    return.warning.message<-DSI::datashield.aggregate(datasources, calltext1)

    calltext2 <- call("recodeValuesDS2", var.name, values2replace.transmit, new.values.transmit,numeric.output.format.possible,force.output.format,v2r.numeric)
    DSI::datashield.assign(datasources, newobj, calltext2)

    numsources<-length(datasources)
    for(s in 1:numsources){
	num.messages<-length(return.warning.message[[s]])
        if (notify.of.progress)
        {
	    if(num.messages==1){
                cat("\nSource",s,"\n",return.warning.message[[s]][[1]],"\n\n")
            }else{
                cat("\nSource",s,"\n")
	        for(m in 1:(num.messages-1)){
	            cat(return.warning.message[[s]][[m]],"\n")
	        }
	        cat(return.warning.message[[s]][[num.messages]],"\n\n")
            }
	}
    }

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
#ds.recodeValues
