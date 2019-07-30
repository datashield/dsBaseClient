#' @title ds.matrixDet calling assign function matrixDetDS2
#' @description Calculates the determinant of a square matrix A 
#' and writes it as a data object to the serverside
#' @details Calculates the determinant of a square matrix (for additional
#' information see help for {determinant} function in native R). This operation is only
#' possible if the number of columns and rows of A are the same.
#' @param M1  A character string specifying the name of the matrix for which
#' determinant to be calculated
#' @param newobj A character string specifying the name of the matrix to which the output
#' is to be written. If no <newobj> argument is specified, the output matrix names defaults
#' to "M1_det" where <M1> is the first argument of the function
#' @param logarithm logical. Default is FALSE, which returns the
#' determinant itself, TRUE returns the logarithm of the modulus of the determinant.
#' @param datasources specifies the particular opal object(s) to use. If the <datasources>
#' argument is not specified the default set of opals will be used. The default opals
#' are called default.opals and the default can be set using the function
#' {ds.setDefaultOpals.o}. If the <datasources> is to be specified, it should be set without
#' inverted commas: e.g. datasources=opals.em or datasources=default.opals. If you wish to
#' apply the function solely to e.g. the second opal server in a set of three,
#' the argument can be specified as: e.g. datasources=opals.em[2].
#' If you wish to specify the first and third opal servers in a set you specify:
#' e.g. datasources=opals.em[c(1,3)]
#' @return the calculated determinant of the matrix as a serverside object
#' with its name specified by the <newobj> argument (or default name <M1>_det).
#' The determinant is reported as a two component list. Element 1
#' is $modulus and element 2 is $sign. If logarithm=FALSE: $modulus reports the
#' absolute value of the determinant and is therefore always positive. $sign 
#' indicates whether the determinant is positive ($sign=1) or negative ($sign=-1).
#' $modulus has an attribute [attr(,"logarithm")] which is FALSE if the argument
#' <logarithm> was FALSE - this enables you to look at results post-hoc to determine
#' whether the logarithm argument was TRUE or FALSE. If you wish to generate the
#' actual determinant if logarithm=FALSE it is easiest to calculate $modulus*$sign.   
#' If logarithm=TRUE: $modulus reports the log (to base e) of the absolute value
#' of the determinant. $sign again reports whether the determinant is positive
#' ($sign=1) or negative ($sign=-1). The attribute of $modulus [attr(,"logarithm")]
#' is now TRUE. If you wish to generate the actual determinant when logarithm=TRUE
#' you calculate exp($modulus)*$sign. In addition to the calculated matrix
#' determinant, two validity messages are also returned
#' indicating whether <newobj> has been created in each data source and if so whether
#' it is in a valid form. If its form is not valid in at least one study - e.g. because
#' a disclosure trap was tripped and creation of the full output object was blocked -
#' ds.matrixDet also returns any studysideMessages that can explain the error in creating
#' the full output object. As well as appearing on the screen at run time,if you wish to
#' see the relevant studysideMessages at a later date you can use the {ds.message.o}
#' function. If you type ds.message.o("newobj") it will print out the relevant
#' studysideMessage from any datasource in which there was an error in creating <newobj>
#' and a studysideMessage was saved. If there was no error and <newobj> was created
#' without problems no studysideMessage will have been saved and ds.message.o("newobj")
#' will return the message: "ALL OK: there are no studysideMessage(s) on this datasource".
#' @author Paul Burton for DataSHIELD Development Team
#' @export
#'
ds.matrixDet<-function(M1=NULL, newobj=NULL, logarithm=FALSE, datasources=NULL){
   
  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }
  
  # check if user has provided the name of matrix representing M1
  if(is.null(M1)){
    return("Error: Please provide the name of the matrix representing M1")
  }

  # if no value or invalid value specified for logarithm, then specify a default
  if(is.null(logarithm))
  {
  logarithm<-FALSE
  }
  
  if(logarithm!=TRUE)
  {
  logarithm<-FALSE
  }
  
  
  # if no value specified for output object, then specify a default
  if(is.null(newobj)){
    newobj <- paste0(M1,"_det")
  }


  # CALL THE MAIN SERVER SIDE ASSIGN FUNCTION
  calltext <- call("matrixDetDS2", M1, logarithm)
  opal::datashield.assign(datasources, newobj, calltext)

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
calltext <- call("testObjExistsDS.o", test.obj.name)													 	#
																											#
object.info<-opal::datashield.aggregate(datasources, calltext)												 	#
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
	if(object.info[[j]]$test.obj.class=="ABSENT"){														 	#
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
	calltext <- call("messageDS.o", test.obj.name)															#
    studyside.message<-opal::datashield.aggregate(datasources, calltext)											#
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
#END OF CHECK OBJECT CREATED CORRECTLY MODULE															 	#
#############################################################################################################

}
#ds.matrixDet
