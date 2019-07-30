#' 
#' @title ds.make.o
#' @description Makes (calculates) a new object in the R environment on the
#' server side. ds.make is equivalent to ds.assign, but runs slightly faster.
#' It defines a datashield object via an allowed function or an arithmetic
#' expression hence creating a new object
#' in the server side R environments. The function is a wrapper for
#' the 'opal' package function 'datashield.assign'.
#' @details If no newobj name is provided, the new object is named 'newObject' by default,
#' otherwise the name can be specified using the newobj argument.
#' If the newObject is created successfully, the function will verify its
#' existence on the required servers. Please note there are certain modes of failure
#' where it is reported that the object has been created but it is not there. This obviously
#' reflects a failure in the processing of some sort and warrants further exploration of the
#' details of the call to ds.make.o and the variables/objects which it invokes.
#' TROUBLESHOOTING: please note we have recently identified an error that makes ds.make.o
#' fail and DataSHIELD crash.  The error arises from a call
#' such as ds.make.o('5.3 + beta*xvar', 'predvals'). This is a typical call you
#' may make to get the predicted values from a simple linear regression model where
#' a y variable is regressed against an x variable (xvar) where the estimated regression
#' intercept is 5.3 and beta is the estimated regression slope. This call appears to
#' fail because in interpreting the arithmetic function which is its first argument
#' it first encounters the (length 1) scalar 5.3 and when it then encounters the xvar vector
#' which has more than one element it fails - apparently because it does not recognise
#' that you need to replicate the 5.3 value the appropriate number of times
#' to create a vector
#' of length equal to xvar with each value equal to 5.3. There are two work-around
#' solutions here: (1) explicitly create a vector of appropriate length with each
#' value equal to 5.3. In order to do this there is a useful trick. First identify
#' a convenient numeric variable with no missing values (typically a numeric
#' individual ID) let us call it indID equal in length to xvar (xvar may include NAs
#' but that doesn't matter provided indID is the same total length). Then issue the call
#' ds.make.o('indID-indID+1','ONES'). This creates a vector of ones (called 'ONES')
#' in each source equal in length to the indID vector in that source. Then issue
#' the second call ds.make.o('ONES*5.3','vect5.3') which creates the required
#' vector of length equal to xvar with all elements 5.3. Finally, you can
#' now issue a modified call to reflect what was originally needed:
#' ds.make.o('vect5.3+beta*xvar', 'predvals'). Alternatively, if you simply
#' swap the original call around: ds.make.o('(beta*xvar)+5.3', 'predvals')
#' the error seems also to be circumvented. This is presumably because the first element
#' of the arithmetic function is of length equal to xvar and it then knows to
#' replicate the 5.3 that many times in the second part of the expression.
#' The second work-around is obviously easier, but it is worth knowing about the 
#' first trick because creating a vector of ones of equal length to another vector
#' can be useful in other settings. Equally the call:
#' ds.make.o('indID-indID','ZEROS') to create a vector of zeros of that same
#' length may also be useful.
#' @param toAssign a character string specifying the function call or the arithmetic expression
#' that generates the newObject. In general the string should be reasonably simple to avoid blocking by the parser
#' and complex (many brackets) expressions can always be broken down into a series of simple steps - e.g.
#' see example 1 below. If toAssign is a simple pre-existing data object, it will simply be copied and assigned as having a second name
#' as specified by the newobject argument - e.g. see example 1 below. One bug identified
#' @param newobj the name of the new object
#' @param datasources specifies the particular opal object(s) to use, if it is not specified
#' the default set of opals will be used. The default opals are always called default.opals.
#' This parameter is set without inverted commas: e.g. datasources=opals.em or datasources=default.opals
#' If you wish to specify the second opal server in a set of three, the parameter is specified:
#' e.g. datasources=opals.em[2]. If you wish to specify the first and third opal servers in a set specify:
#' e.g. datasources=opals.em[2,3]
#' @return the object specified by the newobj argument (or default name newObject) is written to the
#' serverside and a validity message indicating whether the newobject has been correctly
#' created at each source is returned to the client. If it has not been correctly created the return object
#' return.info details in which source the problem exists and whether: (a) the object exists at all; (b) it has meaningful
#' content indicated by a valid class. 
#' @author DataSHIELD Development Team
#' @export
#' @examples
#' \dontrun{
#'
#' ##EXAMPLE 1
#' ##CONVERT PROPORTIONS IN prop.rand TO log(odds) IN logodds.rand
#' #ds.make.o("(prop.rand)/(1-prop.rand)","odds.rand")
#' #ds.make.o("log(odds.rand)","logodds.rand")
#' 
#' 
#'
#' ##EXAMPLE 2
#' ##MISCELLANEOUS ARITHMETIC OPERATORS: ARBITRARY CALCULATION
#' ##USE DEFAULT NEW OBJECT NAME
#' #ds.make.o("((age.60+bmi.26)*(noise.56-pm10.16))/3.2")
#' 
#' 
#'
#' ##EXAMPLE 3
#' ##MISCELLANEOUS OPERATORS WITHIN FUNCTIONS (female.n is binary 1/0 so female.n2 = female.n
#' ##and so they cancel out in code for second call to ds.make.o and so that call is
#' ##equivalent to copying log.surv to output.test.1)  
#' #ds.make.o("female.n^2","female.n2")
#' #ds.make.o("(2*female.n)+(log.surv)-(female.n2*2)","output.test.1") 
#' #ds.make.o("exp(output.test.1)","output.test")
#' }
#' 
ds.make.o<-function(toAssign=NULL, newobj="newObject", datasources=NULL){
  
  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }

  
  if(is.null(toAssign)){
    stop("Please give the name of object to assign or an expression to evaluate and assign.!\n", call.=FALSE)
  }
  
  # now do the business
  opal::datashield.assign(datasources, newobj, as.symbol(toAssign))

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

# ds.make.o  

  
