#' @title ds.seq calling assign function seqDS
#' @description ds.seq calling assign function seqDS
#' @details Calls an assign function that uses the native R function seq() to create
#' any one of a flexible range of sequence vectors that can then be used to help
#' manage and analyse data. As it is an assign function the resultant vector is
#' written as a new object into all of the specified data source servers. Please
#' note that some combinations  of arguments are not allowed for the function {seq}
#' in native R and hence are also prohibited in {ds.seq}. To be specific,
#' FROM.value.char defines the start of the sequence and BY.value.char defines how
#' the sequence is incremented (or decremented) at each step. But where the
#' sequence stops can be defined in three different ways: TO.value.char indicates
#' the terminal value of the sequence e.g. ds.seq(FROM.value.char="3", BY.value.char="2",
#' TO.value.char="7") creates the sequence 3,5,7 on the serverside;
#' LENGTH.OUT.value.char indicates the length of the sequence
#' e.g. ds.seq(FROM.value.char="3", BY.value.char="2", LENGTH.OUT.value.char="7")
#' creates the sequence 3,5,7,9,11,13,15 on the serverside; ALONG.WITH.name
#' specifies the name of a variable (on the serverside) such that the sequence
#' in each study will be equal in length to that variable.
#' i.e. ds.seq(FROM.value.char="3", BY.value.char="2", ALONG.WITH.name="var.x")
#' will create a sequence such that if var.x is of length 100 in study 1 the
#' sequence written to study 1 will be 3,5,7,...,197,199,201 and if var.x is
#' of length 4 in study 2, the sequence written to study 2 will be 3,5,7,9.
#' BUT CRUCIALLY - only one of the three arguments TO.value.char,
#' LENGTH.OUT.value.char and ALONG.WITH.name can be non-null in any one call.
#' @param FROM.value.char the starting value for the sequence expressed as an integer
#' or real number with a decimal point but 
#' in character form. e.g. FROM.value.char="1" will start at 1, FROM.value.char="-10.7"
#' will start at -10.7. Default = "1"
#' @param TO.value.char the terminal value for the sequence expressed as an integer
#' or real number with a decimal point but 
#' in character form. e.g. if TO.value.char="17.33" the sequence will terminate
#' at the last value in the sequence that is <= 17.33. Default = "1"
#' @param BY.value.char the value to increment each step in the sequence
#' expressed as an integer
#' or real number with a decimal point but 
#' in character form. e.g. if BY.value.char="2.93" the sequence will increment
#' by 2.93 at each step, if BY.value.char="-1" the sequence will decrement
#' by 1 at each step. Default = "+1"
#' @param LENGTH.OUT.value.char length of the sequence at which point
#' its extension should be stopped, expressed as an integer
#' or real number with a decimal point but 
#' in character form. e.g.  LENGTH.OUT.value.char="1000" will
#' generate a sequence of length 1000. If however, LENGTH.OUT.value.char
#' is not an integer any value larger than a given integer will result
#' in a sequence of length integer+1. e.g.  LENGTH.OUT.value.char="1000.0001" will
#' generate a sequence of length 1001. If 0.0>=LENGTH.OUT.value.char>(-1.0), the function
#' will run but will generate a sequence with no elements.
#' If LENGTH.OUT.value.char<=(-1.0) the function will terminate with an error.
#' Default = NULL.
#' @param ALONG.WITH.name For convenience, rather than specifying a value
#' for LENGTH.OUT it can often be better to specify a variable name as
#' the <ALONG.WITH.name> argument. e.g. ALONG.WITH.name = "vector.name".
#' This can be particularly useful in DataSHIELD
#' where the length of the sequence you need to generate in each data set
#' depends on the standard length of vectors in that data set and this will
#' in general vary.
#' @param newobj This a character string providing a name for the output
#' sequence vector which defaults to 'newObj' if no name is specified.
#' @param datasources specifies the particular opal object(s) to use. If the <datasources>
#' argument is not specified the default set of opals will be used. The default opals
#' are called default.opals and the default can be set using the function
#' {ds.setDefaultOpals}. If the <datasources> is to be specified, it should be set without
#' inverted commas: e.g. datasources=opals.em or datasources=default.opals. If you wish to
#' apply the function solely to e.g. the second opal server in a set of three,
#' the argument can be specified as: e.g. datasources=opals.em[2].
#' If you wish to specify the first and third opal servers in a set you specify:
#' e.g. datasources=opals.em[c(1,3)].
#' @return the object specified by the <newobj> argument (or default name newObj)
#' which is written to the serverside. 
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
#' and a studysideMessage was saved. If there was no error and <newobj> was created
#' without problems no studysideMessage will have been saved and ds.message("<newobj>")
#' will return the message: "ALL OK: there are no studysideMessage(s) on this datasource".
#' @author Paul Burton for DataSHIELD Development Team, 17/9/2019
#' @export
ds.seq<-function(FROM.value.char = "1", BY.value.char = "1", TO.value.char=NULL, LENGTH.OUT.value.char = NULL, ALONG.WITH.name=NULL,
                   newobj="newObj", datasources=NULL) {


###datasources
  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }

  
###FROM.value.char
  # check FROM.value.char is valid
  FROM.valid<-1
  if(!(is.null(FROM.value.char))) {
		if(!is.character(FROM.value.char))FROM.valid<-0
		if(!is.numeric(eval(parse(text=FROM.value.char))))FROM.valid<-0
	}
	if(!FROM.valid){
  return("Error: If FROM.value.char is non.NULL, it must be a real number in inverted commas eg '-3.7' or '0'")
	}

###TO.value.char
  # check TO.value.char is valid
  TO.valid<-1
  if(!(is.null(TO.value.char))) {
		if(!is.character(TO.value.char))TO.valid<-0
		if(!is.numeric(eval(parse(text=TO.value.char))))TO.valid<-0
	}
	if(!TO.valid){
  return("Error: If TO.value.char is non.NULL, it must be a real number in inverted commas eg '-3.7' or '0'")
	}

###BY.value.char
  # check BY.value.char is valid
  BY.valid<-1
  if(!(is.null(BY.value.char))) {
		if(!is.character(BY.value.char))BY.valid<-0
		if(!is.numeric(eval(parse(text=BY.value.char))))BY.valid<-0
	}
	if(!BY.valid){
  return("Error: If FROM.value.char is non.NULL, it must be a real number in inverted commas eg '5' or '-98.7321'")
	}

###LENGTH.OUT.value.char
  # check LENGTH.OUT.value.char is valid
	LENGTH.OUT.valid<-1
  if(!(is.null(LENGTH.OUT.value.char))) {
		if(!is.character(LENGTH.OUT.value.char))LENGTH.OUT.valid<-0
		if(!is.numeric(eval(parse(text=LENGTH.OUT.value.char))))LENGTH.OUT.valid<-0
	}
	if(!LENGTH.OUT.valid){
  return("Error: If LENGTH.OUT.value.char is non.NULL, it must be an integer in inverted commas eg '87187'")
	}

  
###ALONG.WITH.name
  # check if user has correctly provided the name of a column to hold ALONG.WITH.name
  if(!(is.null(ALONG.WITH.name) || is.character(ALONG.WITH.name))){
    return("Error: If ALONG.WITH.name is non.NULL, it must specify the name of a serverside vector in inverted commas")
	}

###Either LENGTH.OUT.value.char or ALONG.WITH.name must be non-NULL
if(is.null(TO.value.char)&&is.null(LENGTH.OUT.value.char)&&is.null(ALONG.WITH.name)){
    return("Error: Either TO.value.char or LENGTH.OUT.value.char or ALONG.WITH.name must be non-NULL, they cannot both be NULL")
	}



# CALL THE PRIMARY SERVER SIDE FUNCTION
  calltext <- call("seqDS", FROM.value.char,TO.value.char,BY.value.char,LENGTH.OUT.value.char,ALONG.WITH.name)
 opal::datashield.assign(datasources, newobj, calltext)


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
	calltext <- call("messageDS", test.obj.name)															#
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
#ds.seq

