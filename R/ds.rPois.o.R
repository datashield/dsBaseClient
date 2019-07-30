#' @title ds.rPois.o calling rPoisDS.o and setSeedDS.o
#' @description Generates random (pseudorandom) numbers (non-negative integers)
#' with a Poisson distribution
#' @details An assign function that creates a vector of pseudorandom numbers
#' in each data source. This function generates random numbers distributed with a
#' Poisson distribution - non-negative integer values with an expected count fully specified
#' by a single argument lambda. In addition, the ds.rPois.o function's
#' arguments specify the length of the output vector in each source.
#' @param samp.size the length of the random number vector to be created in each source.
#' <samp.size> can be a numeric scalar and this then specifies the length of the
#' random vectors in each source to be the same. If it is a numeric vector
#' it enables the random vectors to be of different lengths in each source but the
#' numeric vector must be of length equal to the number of data sources being used.
#' Often, one wishes to generate random vectors of length equal to the length of
#' standard vectors in each source. To do this most easily, issue a command such as:
#' numobs.list<-ds.length('varname',type='split') where varname is an arbitrary
#' vector of standard length in all sources. Then issue command:
#' numobs<-unlist(numobs.list) to make numobs numeric rather than a list. Finally,
#' declare samp.size=numobs as the first argument for the ds.rPois.o function
#' Please note that because (in this case) numobs is a clientside vector it
#' should be specified without inverted commas (unlike the serverside vectors
#' which may be used for the <lambda> argument [see below]).
#' @param lambda a numeric scalar specifying the expected count of the Poisson
#' distribution used to generate the random counts. If you wish to specify a
#' different value in each source, then you can use the <datasources> argument
#' to create the random vectors one source at a time, changing lambda as required.
#' Alternatively you can specify the <lambda> argument to be a serverside vector
#' equal in length to the random number vector you want to generate and this allows
#' lambda to vary by observation in the dataset. If you wish to specify
#' a serverside vector in this way (e.g. called vector.of.lambdas) you must
#' specify the argument as a character string (..., lambda="vector.of.lambdas"...).
#' If you simply wish to specify a single but different value in each
#' source, then you can specify <lambda> as a scalar and use the
#' <datasources> argument to create the random vectors one source at a time.
#' Default value for <lambda> = 1.
#' @param newobj This a character string providing a name for the output
#' random number vector which defaults to 'newObject' if no name is specified.
#' @param seed.as.integer a numeric scalar or a NULL which primes the random seed
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
#' source-specific seed multiplier will also be 1. The function ds.rPois.o
#' calls the serverside assign function setSeedDS.o to create the random seeds in
#' each source
#' @param return.full.seed.as.set logical, if TRUE will return the full
#' random number seed in each data source (a numeric vector of length 626). If
#' FALSE it will only return the trigger seed value you have provided: eg if
#' <seed.as.integer> = 32 and there are three studies, the ds.rPois.o function will
#' return: "$integer.seed.as.set.by.source", [1]  32  64 96, rather than the three
#' vectors each of length 626 that represent the full seeds generated in each source.
#' Default is FALSE.
#' @param datasources specifies the particular opal object(s) to use. If the <datasources>
#' argument is not specified the default set of opals will be used. The default opals
#' are called default.opals and the default can be set using the function
#' {ds.setDefaultOpals.o}. If the <datasources> is to be specified, it should be set without
#' inverted commas: e.g. datasources=opals.em or datasources=default.opals. If you wish to
#' apply the function solely to e.g. the second opal server in a set of three,
#' the argument can be specified as: e.g. datasources=opals.em[2].
#' If you wish to specify the first and third opal servers in a set you specify:
#' e.g. datasources=opals.em[c(1,3)]
#' @return Writes the pseudorandom number vector with the characteristics specified
#' in the function call as a new serverside vector in each data source. Also returns
#' key information to the clientside: the random seed trigger as specified by you in each
#' source + (if requested) the full 626 length random seed vector this generated in
#' each source (see info for the argument <return.full.seed.as.set>). The ds.rPois.o
#' function also returns a vector reporting the length of the pseudorandom vector
#' created in each source.
#' @author Paul Burton for DataSHIELD Development Team
#' @export
ds.rPois.o<-function(samp.size=1,lambda=1, newobj="newObject", seed.as.integer=NULL, return.full.seed.as.set=FALSE, datasources=NULL){

##################################################################################
# if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
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

###################################################################################
#samp.size is either a numeric scalar or a numeric vector set
#by the user or derived as explained for "numobs" in the help
#for the samp.size parameter - and declared without inverted commas
#as it is a client-side vector lambda is specified either as a numeric scalar,
#or as a vector on the serverside which contains values that
#can vary from row to row of a dataset.
#Such serverside vectors must be named in inverted commas. These get
#stripped off when the vector name is passed to the serverside.

arguments.valid<-1

if(is.null(samp.size)||is.null(lambda)||is.null(newobj)){
arguments.valid<-0
}

if(!arguments.valid){
mess2<-("ERROR: appropriate values must be set for samp.size, lambda, and newobj name")
return(mess2)
}

lambda.valid<-1
if(is.numeric(lambda)){
	if(lambda<=0){
		lambda.valid<-0
	}
}
	
if(!lambda.valid){
mess3<-("ERROR: lambda must be > 0")
return(mess3)	
}
###################################################################################


#######################
#SET SEED IN EACH STUDY

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
cat("NO SEED SET IN STUDY",study.id,"\n\n")

}
  calltext <- paste0("setSeedDS.o(", seed.as.text, ")")
  ssDS.obj[[study.id]] <- opal::datashield.aggregate(datasources[study.id], as.symbol(calltext))
} 
cat("\n\n")





##############################
#GENERATE PSEUDORANDOM NUMBERS

if(length(samp.size)==1){
samp.size<-rep(samp.size,numsources)
}

for(k in 1:numsources){

toAssign<-paste0("rPoisDS.o(",samp.size[k],",",lambda, ")")


  if(is.null(toAssign)){
    stop("Please give the name of object to assign or an expression to evaluate and assign.!\n", call.=FALSE)
  }

  # now do the business
 
  opal::datashield.assign(datasources[k], newobj, as.symbol(toAssign))
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
	if(no.errors && !return.full.seed.as.set){																#
	validity.check<-paste0("<",test.obj.name, "> appears valid in all sources")							    #
	return(list(integer.seed.as.set.by.source=single.integer.seed,random.vector.length.by.source=samp.size, #
	            is.object.created=return.message,validity.check=validity.check))							#
	}																										#
																											#
	if(no.errors && return.full.seed.as.set){																#
	validity.check<-paste0("<",test.obj.name, "> appears valid in all sources")							    #
	return(list(full.seed.as.set=ssDS.obj,																	#
				integer.seed.as.set.by.source=single.integer.seed,random.vector.length.by.source=samp.size, #
	            is.object.created=return.message,validity.check=validity.check))							#
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

#ds.rPois.o

