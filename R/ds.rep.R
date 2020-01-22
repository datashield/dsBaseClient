#' @title ds.rep calling assign function repDS
#' @description Creates a repetitive sequence by repeating
#' an identified scalar, or specified elements of a vector
#' or list. This is analogous to the {rep} function in native R.
#' The sequence is written as a new object to the serverside.
#' @details With four exceptions, ds.rep is almost equivalent to {rep}
#' in native R and the controlling arguments are similar.
#' The four exceptions*** are: (1) the argument <x> in {rep} is called
#' <x1> in {ds.rep}, it is defaulted to NULL and if it is not specified
#' or is called as NULL it fails with an error message;
#' (2) each of the control arguments <x1>,<times> etc
#' is accompanied by a source argument, <x1.source>,
#' <times.source> etc which specifies whether the defining scalar,
#' vector or list is located on the clientside or serverside;
#' (3) if <length.out.source> indicates a serverside vector,
#' the length of the vector named as the <length.out> argument,
#' defines the length of the generated repetitive sequence. This
#' is useful when you want to create a repetitive sequence of the
#' same length as the standard vector length in each study. In native
#' R, the length.out argument of {rep} cannot be a vector;
#' (4) the Boolean <x1.includes.characters> argument determines
#' whether to coerce the final output sequence to numeric.
#' If it truly is numeric this has advantages, but if there are
#' any character elements in the vector this generates NAs. If
#' <x1.includes.characters> is then set to TRUE,
#' the output sequence remains in character format without NAs but
#' it will not then behave as a numeric so, for example, on sorting
#' it will be sorted alphabetically (which can be confusing)
#' rather than numerically.
#' 
#' All arguments that can denote a clientside vector/scalar or
#' a serverside/vector or scalar (i.e. <x1>,<times>, <length.out>
#' or <each>) should follow the calling formats outlined under
#' the information for parameter <x1> below. All source arguments
#' should specify the source as being clientside or serverside
#' using the calling formats under the information for parameter
#' <x1.source> below. 
#'
#' With these exceptions, the behavior of <ds.rep> is similar
#' to <rep> in native R and the following details from the help
#' in R are therefore valid:
#'  
#' Details from R help for <rep>:
#' 
#' The default behaviour is as if the call was 
#' rep(x, times = 1, length.out = NA, each = 1)
#' Normally just one of the additional arguments is specified, but if 'each' is
#' specified with either of the other two, its replication is performed first, and
#' then that is followed by the replication implied by times or length.out.
#' 
#' If times consists of a single integer, the result consists of the whole input
#' repeated this many times. If times is a vector of the same length as x (after
#' replication by each), the result consists of x[1] repeated times[1] times, x[2]
#' repeated times[2] times and so on. ***Note exception 1 above.
#' 
#' length.out may be given in place of times, in which case x is repeated as many
#' times as is necessary to create a vector of this length. If both are given,
#' length.out takes priority and times is ignored. ***Note exception 3 above.
#' 
#' Non-integer values of times will be truncated towards zero. If times is a
#' computed quantity it is prudent to add a small fuzz or use round. And analogously
#' for each.
#' 
#' @param x1 This argument determines the input scalar, vector or list. Depending
#' on the other arguments specified, x1 may either be a clientside scalar or
#' vector or a serverside scalar or vector. If it is a clientside vector or
#' scalar it may either be defined in the call e.g. ds.rep(x1=c(7:12),...) or
#' ds.rep(x1=61,...) or it may first be created as
#' a named vector or scalar:
#'
#' rep.vect<-c(7:12)
#' ds.rep(x1=rep.vect,...)
#' rep.scalar<-61
#' ds.rep(x1=rep.scalar,...)
#'
#' Please note that in the latter case, clientside vectors
#' MUST NOT be written in inverted commas.
#'
#' If it is a serverside scalar or vector, it must already exist as a defined
#' object on the serverside, and it MUST be written in inverted commas
#' ds.rep(x1="named.serverside.vector",...)
#' ds.rep(x1="named.serverside.scalar",...)
#'
#'
#' @param times May be a scalar or a vector from either the clientside
#' or serverside for behaviour see "details from R help for <rep>" above
#' @param length.out May be a clientside scalar or a serverside scalar
#' or vector. See "details from R help for <rep>" above but if it is
#' a serverside vector exception 3 applies*** (see above) - the
#' <length.out> argument in native R cannot treat a vector in this way
#' @param each May be a clientside or serverside scalar (vectors not
#' allowed).
#' @param source.x1 This defines the source of the scalar or vector defined
#' by the <x1> argument. Four character strings are allowed:
#' "clientside" or "c" and serverside or "s". If the abbreviated strings "c"
#' or "s" are used they are internally converted to "clientside" and
#' "serverside as one of the first steps in the function. If this argument
#' is specified in any way other than one of these four character strings
#' the function will fail with an error message.
#' @param source.times see "param source.x1"
#' @param source.length.out see "param source.x1"
#' @param source.each see "param source.x1"
#' @param x1.includes.characters Boolean parameter determining
#' whether to coerce the final output sequence to numeric. Defaults
#' to FALSE and output is coerced to numeric. If <x1> contains
#' any character elements the <x1.includes.characters> argument
#' should be set to TRUE and the output sequence remains in
#' character format. Exception 4 applies*** (see above).
#' @param newobj A character string specifying the name of the output object
#' to be written to the serverside which will be in the form of a vector
#' containing the requested repetitive sequence. Note this can be of
#' length 1 allowing <ds.rep> to create serverside scalars.
#' If no <newobj> argument is specified, the output object name defaults to
#' "seq.vect".
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login.
#' If the <datasources> the default set of connections will be used: see 
#' \link{datashield.connections_default}. If you wish to
#' apply the function solely to e.g. the second connection server in a set of three,
#' the argument can be specified as: e.g. datasources=connections.em[2].
#' If you wish to specify the first and third connection servers in a set you specify:
#' e.g. datasources=connections.em[c(1,3)]. Using the <datasources> argument and
#' repeated calls to <ds.rep>
#' it is possible to create different repetive sequences in the different data
#' sources which can be particularly helpful for creating IDs - for example
#' ds.rep(x1=1,length.out="vector.with.required.length",source.x1="c",
#' source.length.out ="s",newobj="idstudy",datasources=default.connections[1])
#' ds.rep(x1=2,length.out="vector.with.required.length",source.x1="c",
#, source.length.out ="s",newobj="idstudy",datasources=default.connections[2])
#' ds.rep(x1=3,length.out="vector.with.required.length",source.x1="c",
#' source.length.out ="s",newobj="idstudy",datasources=default.connections[3])
#' will create a vector called idstudy in each of three studies which is of length
#' equal to the length of the vector "vector.with.required.length" in
#' each study consisting all of 1s in study 1, all of 2s in study 2 ....
#' @return the vector containing the specified repetitive sequence
#' and write to the output object defined by the <newobj> argument
#' (or default name seq.vect) which is written to the serverside in 
#' each source. In addition, two validity messages are returned
#' indicating whether <newobj> has been created in each data source and if so whether
#' it is in a valid form. If its form is not valid in at least one study - e.g. because
#' a disclosure trap was tripped and creation of the full output object was blocked -
#' ds.matrixDiag also returns any studysideMessages that can explain the error in creating
#' the full output object. As well as appearing on the screen at run time,if you wish to
#' see the relevant studysideMessages at a later date you can use the {ds.message}
#' function. If you type ds.message("newobj") it will print out the relevant
#' studysideMessage from any datasource in which there was an error in creating <newobj>
#' and a studysideMessage was saved. If there was no error and <newobj> was created
#' without problems no studysideMessage will have been saved and ds.message("newobj")
#' will return the message: "ALL OK: there are no studysideMessage(s) on this datasource".
#' @author Paul Burton for DataSHIELD Development Team, 14/10/2019
#' @export
#'
ds.rep<-function(x1=NULL,  times=NA,  length.out=NA, each=1, 
                source.x1='clientside', source.times=NULL,
				source.length.out=NULL,source.each=NULL,
				x1.includes.characters=FALSE,newobj=NULL,datasources=NULL){
  
  # if no connection login details are provided look for 'connection' objects in the environment
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

 # check if a value has been provided for x1
  if(is.null(x1)){
    return("Error: x1 must have a value which is a character string, a numeric vector on the clientside or a scalar")
  }

  # if no value specified for output object, then specify a default
  if(is.null(newobj))
  {
    newobj <- paste0("seq.vect")
  }

#set up aliases for 'clientside', 'serverside' as 'c' and 's'
if(source.x1=='c')source.x1<-'clientside'
if(source.x1=='s')source.x1<-'serverside'

#must allow for NULL arguments
if(!is.null(source.times))
{
if(source.times=='c')source.times<-'clientside'
if(source.times=='s')source.times<-'serverside'
}

if(!is.null(source.length.out))
{
if(source.length.out=='c')source.length.out<-'clientside'
if(source.length.out=='s')source.length.out<-'serverside'
}

if(!is.null(source.each))
{
if(source.each=='c')source.each<-'clientside'
if(source.each=='s')source.each<-'serverside'
}


 
 
  #########################################################
  #Process 'x1' to make transmittable depending on source.x1
  #########################################################
  #Check that source has been specified
  if(source.x1!="serverside"&&source.x1!="clientside")
  {
  cat("            FAILED: if source.x1 is non-null it must be specified as
  one of the following: 'clientside','serverside','c', or 's'\n\n")
  return('Please respecify')
  }

  #process appropriately for source
  if(source.x1=="clientside")
  {
  x1.transmit<-paste0(as.character(x1),collapse=",")
  }
  
  if(source.x1=="serverside")
  {
  x1.transmit<-x1
  }
 
  ###############################################################
  #Process 'times' to make transmittable depending on source.times
  ###############################################################

 #Check that source has been specified
  if(source.times!="serverside"&&source.times!="clientside"&&!is.null(source.times))
  {
  cat("            FAILED: if source.times is non-null it must be specified as
  one of the following: 'clientside','serverside','c', or 's'\n\n")
  return('Please respecify')
  }


  #process appropriately for source
  if(is.null(source.times))
  {
  times.transmit<-paste0(as.character(times),collapse=",")
  }
  
  else
  {
	if(source.times=="clientside")
	{
	times.transmit<-paste0(as.character(times),collapse=",")
	}
  
	if(source.times=="serverside")
	{
	times.transmit<-times
	}
  }
  
  

  #########################################################################
  #Process 'length.out' to make transmittable depending on source.length.out
  #########################################################################

  #Check that source has been specified
  if(source.length.out!="serverside"&&source.length.out!="clientside"&&!is.null(source.length.out))
  {
  cat("            FAILED: if source.length.out is non-null it must be specified as
  one of the following: 'clientside','serverside','c', or 's'\n\n")
  return('Please respecify')
  }


  #process appropriately for source
  if(is.null(source.length.out))
  {
  length.out.transmit<-paste0(as.character(length.out),collapse=",")
  }
  
  else
  {
	if(source.length.out=="clientside")
	{
	length.out.transmit<-paste0(as.character(length.out),collapse=",")
	}
  
	if(source.length.out=="serverside")
	{
	length.out.transmit<-length.out
	}
  }
  
 
 
  #############################################################
  #Process 'each' to make transmittable depending on source.each
  #############################################################

  
  #Check that source has been specified
  if(source.each!="serverside"&&source.each!="clientside"&&!is.null(source.each))
  {
  cat("            FAILED: if source.each is non-null it must be specified as
  one of the following: 'clientside','serverside','c', or 's'\n\n")
  return('Please respecify')
  }


  #process appropriately for source
  if(is.null(source.each))
  {
  each.transmit<-paste0(as.character(each),collapse=",")
  }
  
  else
  {
	if(source.each=="clientside")
	{
	each.transmit<-paste0(as.character(each),collapse=",")
	}
  
	if(source.each=="serverside")
	{
	each.transmit<-each
	}
  }
  
 
  
# CALL THE MAIN SERVER SIDE FUNCTION

  calltext <- call("repDS", x1.transmit=x1.transmit, times.transmit=times.transmit,
                   length.out.transmit=length.out.transmit, each.transmit=each.transmit,
                   x1.includes.characters=x1.includes.characters,
				   source.x1=source.x1, source.times=source.times,
				   source.length.out=source.length.out, source.each=source.each)

 
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
	if(is.null(object.info[[j]]$test.obj.class) || object.info[[j]]$test.obj.class=="ABSENT"){														 	#
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
#ds.rep


