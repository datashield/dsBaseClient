#' 
#' @title Checking that a correct version of a data object exists on a data source server  
#' @description This function checks that a specified data object exists or has been correctly created on a
#' specified set of data servers (may be only one data server). 
#' @details The data object to search for is defined as a character string to the argument test.obj.name. The set of
#' data servers to search on is provided as the argument datasources. Close copies of the code in this function
#' are embedded into other functions that create an object and you then wish to test whether it has successfully
#' been created e.g. ds.make.o, ds.asFactor.o
#' @param test.obj.name a character string specifying the name of the object to search for e.g. "TID.f" 
#' @param datasources specifies the particular opal object(s) to use, if it is not specified
#' the default set of opals will be used. The default opals are always called default.opals.
#' This parameter is set without inverted commas: e.g. datasources=opals.em or datasources=default.opals
#' If you wish to specify the second opal server in a set of three, the parameter is specified:
#' e.g. datasources=opals.em[2]. If you wish to specify the first and third opal servers in a set specify:
#' e.g. datasources=opals.em[2,3]
#' @return If the specified data object exists and is of a valid class (i.e. is not null) in every one of the
#' datasources specified by the datasources argument,  the function returns a list with a single element
#' $return.message which is of the form: "A valid copy of data object <TID.f> exists in all specified data sources".
#' If the specified object is non-existant in at least one of the specified data sources or it exists but is of
#' class "null", the $return message is of the form: "Error: A valid data object TID.h does NOT exist in ALL specified data sources"
#' "It is either ABSENT and/or has no valid content/class, for details see return.info above". The list return.info then includes
#' a list of each data source with an indication of whether the data object being tested exists at all e.g. for study 3 return of
#' the list object $return.info$study3$test.obj.exists = FALSE implies that the tested object does not exist
#' even as a name in study 3 while return of the list object $return.info$study3$test.obj.class = "ABSENT" implies 
#' that even if the tested object exists as a name in study 3 it does not have a valid class (so contains no data etc)
#' @author Burton PR
#' @export
ds.testObjExists.o <- function(test.obj.name=NULL, datasources=NULL){
   
  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
     datasources <- findLoginObjects()
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
calltext <- call("testObjExistsDS.o", test.obj.name)													 #
																										 #
object.info<-opal::datashield.aggregate(datasources, calltext)												 #
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
	if(object.info[[j]]$test.obj.class=="ABSENT"){														 #
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
    if(object.info[[j]]$test.obj.class=="ABSENT"){
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
#ds.testObjExists.o


