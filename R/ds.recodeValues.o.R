#' @title ds.recodeValues.o calling recodeValuesDS1.o and recodeValuesDS2.o
#' @description Takes specified values of elements in a vector and converts
#' them to a matched set of alternative specified values
#' @details Recodes individual values with new individual values. This can
#' apply to numeric values, character values and NAs. One particular use of
#' ds.recodeValues.o is to convert NAs to an explicit value or vice-versa.
#' Please see ds.Boole.o to recode a RANGE of values with a new value. Please
#' note that if you wish to do no more than replace NAs with a new code (e.g. 999)
#' there is a restriction imposed by the fact that if the <values2replace> argument
#' is specified as c(NA) or NA and the <new.values.vector> argument as c(999) the
#' function will fail because it will not properly interpret the first (all values
#' missing) argument as a valid scalar or vector of length 1. To work around this
#' restriction please specify the new <values2replace> argument as c(x,NA) and the
#' <new.values.vector> argument as c(x,999) where x is one of the other valid
#' (non-missing) levels in the vector to be recoded. This will leave the x values
#' unchanged but because the <values2replace> argument is not all missing it will
#' correctly recognise that there are two values to change one of which happens to
#' be NA to be replaced by 999 and the other will be 'replaced' by its pre-existing value.
#' @param var.name a character string providing the name for the vector representing the
#' variable to be recoded
#' @param values2replace.vector a numeric or character vector specifying the values in the
#' vector specified by the argument <var.name> that are to be replaced by new
#' values as specified in the new.values.vector. Example 1, with the two arguments
#' values2replace.vector=c(0,1,2) and new.values.vector=c(20,27.5,37): 0s in the declared
#' <var.name> will be replaced by the value 20, 1s by 27.5 and 2s by 37. If there are any
#' values in the <var.name> vector other than 0, 1 or 2 they will remain unchanged.
#' Example 2, with the two arguments values2replace.vector=c(0,1,2,NA) and
#' new.values.vector=c("Norm","Overwt", "Obese",999): 0s in the declared
#' <var.name> will be replaced by the character value "Norm", 1s by "Overwt",
#' 2s by "Obese" and NAs by 999. As context to these two examples, these represent
#' the recoding of a grouped BMI variable (taking values 0,1,2 and NA) with the
#' numeric value representing the approximate mean of each group (example 1)
#' and category names and an explicit value for missing (example 2). Each value
#' in <values2replace.vector> can only appear once and the length of <values2replace.vector>
#' must be equal to the length of <new.values.vector>
#' @param new.values.vector a numeric or character vector specifying the new values
#' to which the specified values in the vector <var.name> are to be changed.
#' The length of <new.values.vector> must be equal to the length of
#' <values2replace.vector> but more than one value in the latter can be changed
#' to the same value in the former - so <new.values.vector> can include repeated values
#' @param force.output.format character string. If this is 'numeric' the recoded
#' (output) vector will be numeric - any non-numeric values in the
#' <new.values.vector> will appear as NaN in the recoded (output) vector.
#' If the <force.output.format> argument is declared as 'character' all values
#' in the recoded output vector will be in character format. The
#' <force.output.format> argument defaults to "no" and in that case,
#' if the vector identified by the <values2replace.vector> argument is itself
#' numeric and if all values in the <new.values.vector> are numeric,
#' the recoded output vector will also be numeric. Otherwise, it will be coerced
#' to character format.
#' @param newobj This a character string providing a name for the recoded vector
#' representing the primary output of the ds.recodeValues.o() function.
#' This defaults to '<var.name>_recoded' if no name is specified
#' where <var.name> is the first argument of ds.recodeValues.o()
#' @param datasources specifies the particular opal object(s) to use. If the <datasources>
#' argument is not specified the default set of opals will be used. The default opals
#' are called default.opals and the default can be set using the function
#' {ds.setDefaultOpals.o}. If the <datasources> is to be specified, it should be set without
#' inverted commas: e.g. datasources=opals.em or datasources=default.opals. If you wish to
#' apply the function solely to e.g. the second opal server in a set of three,
#' the argument can be specified as: e.g. datasources=opals.em[2].
#' If you wish to specify the first and third opal servers in a set you specify:
#' e.g. datasources=opals.em[c(1,3)]
#' @param notify.of.progress specifies if console output should be produce to indicate
#' progress. The default value for notify.of.progress is FALSE.
#' @return the object specified by the <newobj> argument (or default name '<var.name>_recoded').
#' which is written to the serverside. In addition, two validity messages are returned
#' indicating whether <newobj> has been created in each data source and if so whether
#' it is in a valid form. If its form is not valid in at least one study - e.g. because
#' a disclosure trap was tripped and creation of the full output object was blocked -
#' the function returns a range of possible studysideMessages that can explain
#' the error in creating
#' the full output object. As well as appearing on the screen at run time,if you wish to
#' see the relevant studysideMessages at a later date you can use the {ds.message.o}
#' function. If you type ds.message.o("newobj") it will print out the relevant
#' studysideMessage from any datasource in which there was an error in creating <newobj>
#' and a studysideMessage was saved. If there was no error and <newobj> was created
#' without problems no studysideMessage will have been saved and ds.message.o("newobj")
#' will return the message: "ALL OK: there are no studysideMessage(s) on this datasource".
#' @author DataSHIELD Development Team
#' @export 

ds.recodeValues.o<-function(var.name=NULL, values2replace.vector=NULL, new.values.vector=NULL, force.output.format="no", newobj=NULL, datasources=NULL, notify.of.progress=FALSE){

   # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
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
in the help information for ds.recodeValues.o to find an easy work around that circumvents
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

 
    calltext1 <- call("recodeValuesDS1.o", var.name, values2replace.transmit, new.values.transmit)
    return.warning.message<-opal::datashield.aggregate(datasources, calltext1)
	
    calltext2 <- call("recodeValuesDS2.o", var.name, values2replace.transmit, new.values.transmit,numeric.output.format.possible,force.output.format,v2r.numeric)
    opal::datashield.assign(datasources, newobj, calltext2)

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
#ds.recodeValues.o

