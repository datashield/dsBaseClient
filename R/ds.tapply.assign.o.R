#' @title ds.tapply.assign.o calling tapplyDS.assign.o
#' @description Apply one of a selected range of functions to summarize an
#' outcome variable over one or more indexing factors and write the resultant
#' summary as an object on the serverside
#' @details A clientside function calling an assign serverside function that uses
#' the native R function tapply() to apply one of
#' a selected range of functions to each cell of a ragged array, that is to each (non-empty)
#' group of values given by each unique combination of a series of indexing factors. The native R
#' {tapply} function is very flexible and the range of allowable summarizing functions
#' is much more restrictive for the DataSHIELD ds.tapply.o function. This is to protect
#' against disclosure risk. At present the allowable functions are: N or length (the number
#' of (non-missing) observations in the group defined by each combination of indexing
#' factors; mean; SD (standard deviation); sum; quantile (with quantile probabilities set at
#' c(0.05,0.1,0.2,0.25,0.3,0.33,0.4,0.5,0.6,0.67,0.7,0.75,0.8,0.9,0.95). Should other functions
#' be required in the future then, provided they are non-disclosive, the DataSHIELD development
#' team could work on them if requested. As an assign function {tapplyDS.assign.o}
#' writes the summarized values to the serverside. Because unlike the aggregate function
#' {tapplyDS.o}, {tapply.assign.o} returns no results to the clientside, it is fundamentally
#' non-disclosive and the number of observations in each unique indexing group does
#' not need to be evaluated against nfilter.tab (the minimum allowable non-zero count
#' in a contingency table). This means that tapplyDS.assign.o can be used, for example, 
#' to break a dataset down into a small number of values for each individual and then to flag up
#' which individuals have got at least one positive value for a binary outcome variable.
#' This will almost inevitably generate some indexing groups smaller than nfilter.tab but
#' as the results are simply written as newobj to the serverside rather than returned to
#' the clientside there is no overt disclosure risk. The native R
#' tapply function has optional arguments such as na.rm=TRUE for FUN = mean which will
#' exclude any NAs from the outcome variable to be summarized. However, in order to keep
#' DataSHIELD's {ds.tapply.o} and {ds.tapply.assign.o} functions straightforward, the
#' serverside functions {tapplyDS.o} and {tapplyDS.assign.o} both start by stripping
#' any observations which have missing (NA) values in either the outcome variable or in
#' any one of the indexing factors. In consequence, the resultant analyses are always based
#' on complete.cases.   
#' @param X.name, the name of the variable to be summarized. The user must set the name as a
#' character string in inverted commas. For example: X.name="var.name"
#' @param INDEX.names, the name of a single factor or a vector of names of factors to
#' index the variable to be summarized. Each name must be specified in inverted commas.
#' For example: INDEX.names="factor.name" or
#' INDEX.names=c("factor1.name", "factor2.name", "factor3.name"). The native R tapply function
#' can coerce non-factor vectors into factors. However, this does not always work when
#' using the DataSHIELD ds.tapply.o/ds.tapply.assign.o functions so if you are concerned that
#' an indexing vector is not being treated correctly as a factor,
#' please first declare it explicitly as a factor using {ds.asFactor.o}   
#' @param FUN.name, the name of one of the allowable summarizing functions to be applied
#' specified in inverted commas. The present version of the
#' function allows the user to choose one of five summarizing functions. These are
#' "N" (or "length"), "mean","sd", "sum", or "quantile". For more information see Details.
#' @param newobj A character string specifying the name of the vector to which the output
#' vector is to be written. If no <newobj> argument is specified, the output vector defaults
#' to "tapply.out".
#' @param datasources specifies the particular opal object(s) to use. If the <datasources>
#' argument is not specified the default set of opals will be used. The default opals
#' are called default.opals and the default can be set using the function
#' {ds.setDefaultOpals.o}. If the <datasources> is to be specified, it should be set without
#' inverted commas: e.g. datasources=opals.em or datasources=default.opals. If you wish to
#' apply the function solely to e.g. the second opal server in a set of three,
#' the argument can be specified as: e.g. datasources=opals.em[2].
#' If you wish to specify the first and third opal servers in a set you specify:
#' e.g. datasources=opals.em[c(1,3)]
#' @return an array of the summarized values created by the tapplyDS.assign.o function.
#' This array is written as a newobj onto the serverside. It has the same number of
#' dimensions as INDEX. 
#' @author Paul Burton, Demetris Avraam for DataSHIELD Development Team
#' @export
ds.tapply.assign.o <- function(X.name=NULL, INDEX.names=NULL, FUN.name=NULL, newobj="tapply.out",datasources=NULL){

  ###datasources
  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }
  
  ###X.name
  # check if user has provided the name of the column that holds X.name
  if(is.null(X.name)){
    return("Error: Please provide the name of the variable to be summarized, as a character string")
  }

  ###INDEX.names
  # check if user has provided the name of the column(s) that holds INDEX.names
  if(is.null(INDEX.names)){
    Err.1 <- "Error: Please provide the name of the single factor or"
    Err.2 <- "the list of factors to index the variable to be summarized."
    Err.3 <- "In either case the argument must be specified in inverted commas"
    return(list(Error.message=Err.1, Err.cont2=Err.2, Err.cont3=Err.3))
  }

 #make INDEX.names transmitable
  if(!is.null(INDEX.names)){
    INDEX.names.transmit <- paste(INDEX.names,collapse=",")
  }else{
    INDEX.names.transmit <- NULL
  }

  ###FUN.name  
  # check if user has provided a valid summarizing function
  if(is.null(FUN.name)){
    return("Error: Please provide a valid summarizing function, as a character string")
  }

    # create a name by default if user did not provide a name for the new tapply object
  if(is.null(newobj)){
    newobj <- "tapply.out"
  }

  # CALL THE PRIMARY SERVER SIDE FUNCTION
  calltext <- call("tapplyDS.assign.o", X.name, INDEX.names.transmit, FUN.name)
 
  opal::datashield.assign(datasources, newobj, calltext)

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
#ds.tapply.assign.o

