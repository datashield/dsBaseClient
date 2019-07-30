#' 
#################################################################################################### 100 hashes

#' @title ds.Boole.o
#' @description Converts the individual elements of a vector or other object into Boolean
#' indicators(TRUE/FALSE or 1/0) based on the standard set of Boolean operators:
#' ==, !=, >, >=, <, <=.
#' @details A combination of operators reflected in AND can be obtained by multiplying two or more
#' binary/Boolean vectors together: observations taking the value 1 in every vector will then
#' take the value 1 while all others will take the value 0. The combination OR can be obtained by
#' adding two or more vectors and then then reapply ds.Boole.o using the operator >= 1: any
#' observation taking the value 1 in one or more vectors will take the value 1 in the final vector.
#' @param V1 A character string specifying the name of the vector to which the Boolean operator
#' is to be applied
#' @param V2 A character string specifying the name of the vector or scalar to which <V1> is to
#' be compared. So, if <V2> is a scalar (e.g. '4') and the Boolean operator is '<=', the
#' output vector will be a binary/Boolean variable with elements taking the value 1 or TRUE
#' if the corresponding element of <V1> is 4 or less and 0 or FALSE otherwise. On the other
#' hand, if <V2> is a vector and the Boolean operator is '==', the output vector will be a
#' binary/Boolean variable with elements taking the value 1 or TRUE if the corresponding
#' elements of <V1> and <V2> are equal and 0 or FALSE otherwise. If <V2> is a vector rather than
#' a scalar it must be of the same length as <V1>
#' @param Boolean.operator A character string specifying one of six possible Boolean operators:
#' '==', '!=', '>', '>=', '<', '<='
#' @param numeric.output a TRUE/FALSE indicator defaulting to TRUE determining whether the final
#' output variable should be of class numeric (1/0) or class logical (TRUE/FALSE). It is easy
#' to convert a logical class variable to numeric using the ds.asNumeric() function and to
#' convert a numeric (1/0) variable to logical you can apply ds.Boole.o with <Boolean.operator>
#' '==', <V2> the scalar '1' and <numeric.output> FALSE. 
#' @param na.assign A character string taking values 'NA', '1' or '0'. If 'NA' then any NA
#' values in the input vector remain as NAs in the output vector. If '1' or '0' NA values in
#' the input vector are all converted to 1 or 0 respectively.
#' @param newobj A character string specifying the name of the vector to which the output
#' vector is to be written. If no <newobj> argument is specified, the output vector defaults
#' to "V1_Boole" where <V1> is the first argument of the function.
#' @param datasources specifies the particular opal object(s) to use. If the <datasources>
#' argument is not specified the default set of opals will be used. The default opals
#' are called default.opals and the default can be set using the function
#' {ds.setDefaultOpals.o}. If the <datasources> is to be specified, it should be set without
#' inverted commas: e.g. datasources=opals.em or datasources=default.opals. If you wish to
#' apply the function solely to e.g. the second opal server in a set of three,
#' the argument can be specified as: e.g. datasources=opals.em[2].
#' If you wish to specify the first and third opal servers in a set you specify:
#' e.g. datasources=opals.em[c(1,3)]
#' @return the object specified by the <newobj> argument (or default name <V1>_Boole)
#' which is written to the serverside. In addition, two validity messages are returned
#' indicating whether <newobj> has been created in each data source and if so whether
#' it is in a valid form. If its form is not valid in at least one study - e.g. because
#' a disclosure trap was tripped and creation of the full output object was blocked -
#' ds.Boole.o also returns any studysideMessages that can explain the error in creating
#' the full output object. As well as appearing on the screen at run time,if you wish to
#' see the relevant studysideMessages at a later date you can use the {ds.message.o}
#' function. If you type ds.message.o("newobj") it will print out the relevant
#' studysideMessage from any datasource in which there was an error in creating <newobj>
#' and a studysideMessage was saved. If there was no error and <newobj> was created
#' without problems no studysideMessage will have been saved and ds.message.o("newobj")
#' will return the message: "ALL OK: there are no studysideMessage(s) on this datasource".
#' @author DataSHIELD Development Team
#' @export
#'
ds.Boole.o<-function(V1=NULL, V2=NULL, Boolean.operator=NULL, numeric.output=TRUE, na.assign="NA",newobj=NULL, datasources=NULL){
  
  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }
  
  # check if user has provided the name of the column or scalar that holds V1
  if(is.null(V1)){
    stop("Please provide the name of the column or scalar that holds V1!", call.=FALSE)
  }

  # check if user has provided the name of a column or scalar holding V2 or has declared a scalar value: eg '3'
  if(is.null(V2)){
    stop("Please provide the name of a column or scalar holding V2 or declare a scalar in character format: eg '3'", call.=FALSE)
  }

  # check if user has provided a Boolean operator in character format: eg '==' or '>=' or '<' or '!='
  if(is.null(Boolean.operator)){
    stop("Please provide a Boolean operator in character format: eg '==' or '>=' or '<' or '!='", call.=FALSE)
  }
  
  #check if na.assign has legal value
  if(!(na.assign=="NA"||na.assign=="0"||na.assign=="1")){
    stop("Error: na.assign must be a character string taking value 'NA', '0' or '1'- if <na.action> not specified default is 'NA'", call.=FALSE)
  }
  
  

#convert Boolean operator to numeric

BO.n<-0
if(Boolean.operator == "=="){
   BO.n<-1
}

if(Boolean.operator == "!="){
   BO.n<-2
}

if(Boolean.operator == "<"){
   BO.n<-3
}

if(Boolean.operator == "<="){
   BO.n<-4
}

if(Boolean.operator == ">"){
   BO.n<-5
}

if(Boolean.operator == ">="){
   BO.n<-6
}

  # if no value spcified for output object, then specify a default
  if(is.null(newobj)){
    newobj <- paste0(V1,"_Boole")
  }

# CALL THE MAIN SERVER SIDE FUNCTION
  calltext <- call("BooleDS.o", V1, V2, BO.n, na.assign,numeric.output)
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
#END OF CHECK OBJECT CREATED CORECTLY MODULE															 	#
#############################################################################################################

}
#ds.Boole.o

