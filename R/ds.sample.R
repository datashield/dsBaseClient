#ds.sample
#' @title Performs random sampling and permuting of vectors, dataframes and matrices
#' @description draws a pseudorandom sample from a vector, dataframe or matrix
#' on the serverside
#' or - as a special case - randomly permutes a vector, dataframe or matrix.
#' @details Clientside function ds.sample calls serverside
#' assign function sampleDS. Based on the native R function \code{sample()} but deals
#' slightly differently with data.frames and matrices. Specifically the \code{sample()}
#' function in R identifies the length of an object and then samples n components
#' of that length. But length(data.frame) in native R returns the number of columns
#' not the number of rows. So if you have a data.frame with 71 rows and 10 columns,
#' the sample() function will select 10 columns at random, which is often not what
#' is required. So, ds.sample(x="data.frame",size=10) in DataSHIELD will sample
#' 10 rows at random(with or without replacement depending whether the [replace]
#' argument is TRUE or FALSE, with False being default). If x is a simple vector
#' or a matrix it is first coerced to a data.frame on the serverside and so is dealt
#' with in the same way (i.e. random selection of 10 rows). If x is an integer
#' not expressed as a character string, it is dealt with in exactly the same way
#' as in native R. That is, if x = 923 and size=117, DataSHIELD will draw a 
#' random sample in random order of size 117 from the vector 1:923 (i.e.
#' 1, 2, ... ,923) with or without replacement depending whether [replace] is
#' TRUE or FALSE. If the [x] argument is numeric (e.g. 923) and size is either undefined
#' or set equal to 923, the output on the serverside will be a vector of length 923
#' permuted into a random order. If the [x] argument is a vector, matrix or data.frame on the
#' serverside and if the [size] argument is set either to 0 or to the length of
#' the object to be 'sampled' and [replace] is FALSE, then ds.sample will
#' draw a random sample that includes all rows of the input object but will randomly
#' permute them. This is how ds.sample enables random permuting as well as random
#' sub-sampling. When a serverside vector, matrix or data.frame is sampled using ds.sample
#' 3 new columns are appended to the right of the output object. These are:
#' 'in.sample', 'ID.seq', and 'sampling.order'. The first of these is set to
#' 1 whenever a row enters the sample and as a QA test, all values in that column
#' in the output object should be 1. 'ID.seq' is a sequential numeric ID appended to
#' the right of the object to be sampled during the running of ds.sample that runs from
#' 1 to the length of the object and will be appended even if there is already 
#' an equivalent sequential ID in the object. The output object is stored in
#' the same original order as it was before sampling, and so if the first 
#' four elements of 'ID.seq' are 3,4, 6, 15 ... then it means that rows 1 and 2 were
#' not included in the random sample, but rows 3, 4 were. Row 5 was not included,
#' 6 was included and rows 7-14 were not etc. The 'sampling.order' vector is
#' of class numeric and indicates the order in which the rows entered the sample:
#' 1 indicates the first row sample, 2 the second etc. The lines of code that follow
#' create an output object of the same length as the input object (PRWa)
#' but they join the sample in random order. By sorting the output object (in this
#' case with the default name 'newobj.sample) using ds.dataFrameSort with the
#' 'sampling.order' vector as the sort key, the output object is rendered
#' equivalent to PRWa but with the rows randomly permuted (so the column reflecting
#' the vector 'sample.order' now runs from 1:length of object, while the
#' column reflecting 'ID.seq' denoting the original order is now randomly ordered.
#' If you need to return to the original order you can simply us ds.dataFrameSort
#' again using the column reflecting 'ID.seq' as the sort key:
#' (1) ds.sample('PRWa',size=0,seed.as.integer = 256);
#' (2) ds.make("newobj.sample$sampling.order","sortkey");
#' (3) ds.dataFrameSort("newobj.sample","sortkey",newobj="newobj.permuted")
#' The only additional detail to note is that the original name of the sort key
#' ("newobj.sample$sampling.order") is 28 characters long, and because its
#' length is tested to check for disclosure risk, this original name will
#' fail using the usual value for 'nfilter.stringShort' (i.e. 20). This is
#' why line 2 is inserted to create a copy with a shorter name. 
#' @param x  Either a character string providing the name for the serverside
#' vector, matrix or data.frame to be sampled or permuted, or an integer/numeric
#' scalar (e.g. 923) indicating that one should create a new vector on the serverside
#' that is a randomly permuted sample of the vector 1:923, or (if [replace]
#' = FALSE, a full random permutation of that same vector. For further details
#' of using ds.sample with x set as an integer/numeric please see help for
#' the \code{sample} function in native R. But if x is set as a character string
#' denoting a vector, matrix or data.frame on the serverside, please note
#' that although \code{ds.sample} effectively calls \code{sample} on the serverside
#' it behaves somewhat differently to \code{sample} - for the reasons identified
#' at the top of 'details' and so help for \code{sample} should be used as a guide
#' only.
#' @param size a numeric/integer scalar indicating the size of the sample to
#' be drawn. If the [x] argument is a vector, matrix or data.frame on the
#' serverside and if the [size] argument is set either to 0 or to the length of
#' the object to be 'sampled' and [replace] is FALSE, then ds.sample will
#' draw a random sample that includes all rows of the input object but will randomly
#' permute them. If the [x] argument is numeric (e.g. 923) and size is either undefined
#' or set equal to 923, the output on the serverside will be a vector of length 923
#' permuted into a random order. If the [replace] argument is FALSE then the value
#' of [size] must be no greater than the length of object to be sorted - if
#' this is violated an error message will be returned.
#' @param seed.as.integer this is precisely equivalent to the [seed.as.integer]
#' arguments for the pseudo-random number generating functions (e.g.
#' also see help for ds.rBinom, ds.rNorm, ds.rPois and ds.rUnif).
#' In other words the seed.as.integer argument is either a a numeric scalar
#' or a NULL which primes the random seed
#' in each data source. If <seed.as.integer> is a numeric scalar (e.g. 938)
#' the seed in each study is set as 938*1 in the first study in the set of
#' data sources being used, 938*2 in the second, up to 938*N in the Nth study.
#' If <seed.as.integer> is set as 0 all sources will start with the seed value
#' 0 and all the random number generators will therefore start from the same position.
#' If you want to use the same starting seed in all studies but do not wish it to
#' be 0, you can specify a non-zero scalar value for <seed.as.integer> and then
#' use the <datasources> argument to generate the random number vectors one source at
#' a time (e.g. ,datasources=default.opals[2] to generate the random vector in source 2).
#' As an example, if the <seed.as.integer> value is 78326 then the seed
#' in each source will be set at 78326*1 = 78326 because the vector of datasources
#' being used in each call to the function will always be of length 1 and so the
#' source-specific seed multiplier will also be 1. The function ds.rUnif.o
#' calls the serverside assign function setSeedDS.o to create the random seeds in
#' each source
#' @param replace a Boolean indicator (TRUE or FALSE) specifying whether the
#' sample should be drawn with or without replacement. Default is FALSE so
#' the sample is drawn without replacement. For further details see
#' help for \code{sample} in native R.
#' @param prob a character string containing the name of a numeric vector
#' of probability weights on the serverside that is associated with each of the
#' elements of the vector to be sampled enabling the drawing of a sample
#' with some elements given higher probability of being drawn than others.
#' For further details see help for \code{sample} in native R.
#' @param newobj This a character string providing a name for the output
#' data.frame which defaults to 'newobj.sample' if no name is specified.
#' @param datasources specifies the particular opal object(s) to use. If the <datasources>
#' argument is not specified the default set of opals will be used. The default opals
#' are called default.opals and the default can be set using the function
#' \code{ds.setDefaultOpals}. If the <datasources> is to be specified, it should be set without
#' inverted commas: e.g. datasources=opals.em or datasources=default.opals. If you wish to
#' apply the function solely to e.g. the second opal server in a set of three,
#' the argument can be specified as: e.g. datasources=opals.em[2].
#' If you wish to specify the first and third opal servers in a set you specify:
#' e.g. datasources=opals.em[c(1,3)]
#' @param notify.of.progress specifies if console output should be produce to indicate
#' progress. The default value for notify.of.progress is FALSE.
#' @return the object specified by the <newobj> argument (or default name
#' 'newobj.sample')
#' which is written to the serverside. In addition, two validity messages are returned
#' indicating whether <newobj> has been created in each data source and if so whether
#' it is in a valid form. If its form is not valid in at least one study - e.g. because
#' a disclosure trap was tripped and creation of the full output object was blocked -
#' ds.dataFrameSort() also returns any studysideMessages that may explain the error in creating
#' the full output object. We are currently working to extend the information that can
#' be returned to the clientside when an error occurs.
#' @author Paul Burton, for DataSHIELD Development Team, 15/4/2020
#' @export
ds.sample<-function(x=NULL,  size=NULL, seed.as.integer=NULL, replace=FALSE, prob = NULL, newobj=NULL,datasources=NULL, notify.of.progress=FALSE){
 

  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }

  # check if a value has been provided for x
  if(is.null(x)){
    return("Error: x must denote a character string naming the serverside object to be sampled or an integer N denoting permute 1:N")
  }

 # check if a valid value has been provided for size if x denotes a vector,
 # matrix or data.frame on the serverside
 
  if(is.character(x)&&is.null(size)){
    return("Error: size must have a value which is an integer denoting how many records to be sampled")
  }

  # if no value specified for output object, then specify a default
  if(is.null(newobj))
  {
    newobj <- "newobj.sample"
  }

########################
#TEST SEED PRIMING VALUE
seed.valid<-0

if(is.null(seed.as.integer)){
seed.as.text<-"NULL"
seed.valid<-1
}

if(is.numeric(seed.as.integer)){
seed.as.text<-as.character(seed.as.integer)
seed.valid<-1
}

if(seed.valid==0){
mess1<-("ERROR failed: seed.as.integer must be set as an integer [numeric] or left NULL")
return(mess1)
}

#############################################################
#SET SEED IN EACH STUDY AS IN PSEUDORANDOM NUMBER GENERATORS

ssDS.obj<-list()

numsources<-length(datasources)

single.integer.seed<-NULL

for(study.id in 1:numsources){

if(is.null(seed.as.integer)){
seed.as.text<-"NULL"
}

if(is.numeric(seed.as.integer)){
seed.as.integer.study.specific<-(seed.as.integer*study.id) #if set as 0 all studies will be the same
seed.as.text<-as.character(seed.as.integer.study.specific)
single.integer.seed<-c(single.integer.seed,seed.as.integer.study.specific)
}



if(seed.as.text=="NULL"){
    if (notify.of.progress)
        cat("NO SEED SET IN STUDY",study.id,"\n\n")

}
  calltext <- paste0("setSeedDS(", seed.as.text, ")")
  
    if (notify.of.progress)
      print(calltext)
  
  ssDS.obj[[study.id]] <- DSI::datashield.aggregate(datasources[study.id], as.symbol(calltext))
} 
if (notify.of.progress)
    cat("\n\n")





# CALL THE MAIN SERVER SIDE FUNCTION


calltext <- call("sampleDS", x.transmit=x, size.transmit=size, replace.transmit=replace, prob.transmit=prob)


DSI::datashield.assign(datasources, newobj, calltext)

 
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
calltext <- call("testObjExistsDS", test.obj.name)													 		#
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
#ds.sample



