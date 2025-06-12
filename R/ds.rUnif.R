#' @title Generates Uniform  distribution in the server-side
#' @description Generates uniformly distributed random (pseudorandom) scalar numbers.
#' Besides, \code{ds.rUnif} allows creating different vector lengths in each server.
#' @details It creates a vector of pseudorandom numbers distributed 
#' with a uniform probability in each data source. 
#' The \code{ds.Unif} function's arguments specify 
#' the minimum and maximum of the uniform distribution 
#' and the length and the seed of the output vector in each source.
#' 
#' To specify different \code{min} values in each source, 
#' you can use a character vector \code{(..., min="vector.of.mins"...)}
#' or the \code{datasources} parameter to create the random vector for one source at a time, 
#' changing the \code{min} value as required.
#' Default value for \code{min = 0}. 
#' 
#' To specify different \code{max} values in each source, 
#' you can use a character vector \code{(..., max="vector.of.maxs"...)}
#' or the \code{datasources} parameter to create the random vector for one source at a time, 
#' changing the \code{max} value as required.
#' Default value for \code{max = 1}. 
#' 
#' If \code{seed.as.integer} is an integer 
#' e.g. 5 and there is more than one source (N) the seed is set as 5*N. 
#' For example, in the first study the seed is set as 938*1, 
#' in the second as  938*2  
#' up to 938*N in the Nth study.
#' 
#' If \code{seed.as.integer} is set as 0 all sources will start with the seed value
#' 0 and all the random number generators will, therefore, start from the same position. 
#' Also, to use the same starting seed in all studies but do not wish it to
#' be 0, you can use \code{datasources} argument to generate 
#' the random number vectors one source at  a time. 
#' 
#' In \code{force.output.to.k.decimal.places} the range of k is 1-8 decimals. 
#' If \code{k = 0} the output random numbers are forced to an integer.  
#' If \code{k = 9}, no rounding of output numbers occurs. 
#' The default value of \code{force.output.to.k.decimal.places = 9}.
#' If you wish to generate integers with equal probabilities in the range 1-10
#' you should specify  \code{min = 0.5} and \code{max = 10.5}. 
#' Default value for \code{k = 9}.
#' 
#' Server functions called: \code{rUnifDS} and \code{setSeedDS}.
#' 
#' @param samp.size an integer value or an integer vector that defines the 
#' length of the random numeric vector to be created in each source.
#' @param min a numeric scalar that specifies the minimum value of the 
#' random numbers in the distribution.    
#' @param max a numeric scalar that specifies the maximum value of the 
#' random numbers in the distribution.
#' @param newobj 	a character string that provides the name for the output variable 
#' that is stored on the data servers. Default \code{newObject}. 
#' @param seed.as.integer an integer or a NULL value which provides the random 
#' seed in each data source.
#' @param return.full.seed.as.set logical, if TRUE will return the full random number 
#' seed in each data source (a numeric vector of length 626). If FALSE it will only 
#' return the trigger seed value you have provided. Default is FALSE.
#' @param force.output.to.k.decimal.places an integer or 
#' an integer vector that forces the output random 
#' numbers vector to have k decimals.
#' 
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} objects obtained after login. 
#' If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#' @return \code{ds.Unif} returns random number vectors with a uniform distribution for each study,
#' taking into account the values specified in each parameter of the function.
#' The created vectors are stored in the server-side. If requested, it also returned to the 
#' client-side the full 626 lengths random seed vector generated in each source
#' (see info for the argument \code{return.full.seed.as.set}).
#' @examples 
#' 
#' \dontrun{
#' 
#'   ## Version 6, for version 5 see the Wiki
#'   # Connecting to the Opal servers
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
#'   # Log onto the remote Opal training servers
#'   connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D") 
#'
#'   # Generating the vectors in the Opal servers
#'
#'   ds.rUnif(samp.size = c(12,20,4), #the length of the vector created in each source is different 
#'            min = as.character(c(0,2,5)), #different minumum value of the function in each source
#'            max = as.character(c(2,5,9)), #different maximum value of the function in each source
#'            newobj = "Unif.dist",
#'            seed.as.integer = 234,
#'            return.full.seed.as.set = FALSE,
#'            force.output.to.k.decimal.places = c(1,2,3),
#'            datasources = connections)   #all the Opal servers are used, in this case 3 
#'                                         #(see above the connection to the servers) 
#'
#'   ds.rUnif(samp.size = 12,
#'            min = 0,
#'            max = 2,
#'            newobj = "Unif.dist",
#'            seed.as.integer = 12345,
#'            return.full.seed.as.set = FALSE,
#'            force.output.to.k.decimal.places = 2,
#'            datasources = connections[2]) #only the second  Opal server is used ("study2")
#'            
#'   # Clear the Datashield R sessions and logout           
#'   datashield.logout(connections)
#' }
#'  
#' @author DataSHIELD Development Team
#' @export
ds.rUnif<-function(samp.size=1,min=0,max=1, newobj="newObject", seed.as.integer=NULL, return.full.seed.as.set=FALSE,
                     force.output.to.k.decimal.places=9,datasources=NULL){

##################################################################################
# look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
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
#as it is a client-side vector
#min and max are either numeric scalars, or vectors on the serverside
#which contain values that can vary from row to row of a dataset.
#Such serverside vectors must be named in inverted commas. These get
#stripped off when the vector name is passed to the serverside.

arguments.valid<-1

if(is.null(samp.size)||is.null(min)||is.null(max)||is.null(newobj)){
arguments.valid<-0
}


if(!arguments.valid){
mess2<-("ERROR: appropriate values must be set for samp.size, min, max, and newobj name")
return(mess2)
}


minmax.valid<-1
if(is.numeric(min) && is.numeric(max)){

	if(min>=max){
		minmax.valid<-0
		}

}

if(!minmax.valid){
mess3<-("ERROR: max must be greater than min")
return(mess3)
}

decimal.places.valid<-1
if(force.output.to.k.decimal.places<0||force.output.to.k.decimal.places>9){
decimal.places.valid<-0
}

if(!decimal.places.valid){
mess4<-("ERROR: force.output.to.k.decimal.places must be an integer in range 0-9")
return(mess4)
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
cat("NO SEED SET IN STUDY",study.id,"\n")

} else {
  calltext <- paste0("setSeedDS(", seed.as.text, ")")
  ssDS.obj[[study.id]] <- DSI::datashield.aggregate(datasources[study.id], as.symbol(calltext))
}
} 


##############################
#GENERATE PSEUDORANDOM NUMBERS

if(length(samp.size)==1){
samp.size<-rep(samp.size,numsources)
}

for(k in 1:numsources){

toAssign<-paste0("rUnifDS(",samp.size[k],",",min, ",", max, ",", force.output.to.k.decimal.places,")")


  if(is.null(toAssign)){
    stop("Please give the name of object to assign or an expression to evaluate and assign.!\n", call.=FALSE)
  }

  # now do the business

  DSI::datashield.assign(datasources[k], newobj, as.symbol(toAssign))
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

#ds.rUnif
