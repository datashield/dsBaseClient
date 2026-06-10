#' @title Sub-sets data frames in the server-side
#' @description Subsets a data frame by rows and/or by columns.
#' @details Subset a pre-existing data frame using the standard 
#' set of Boolean operators (\code{==, !=, >, >=, <, <=}). 
#' The  subsetting is made by rows, but it is also possible to select
#' columns to keep or remove. Instead, if you
#' wish to keep all rows in the subset (e.g. if the primary plan is to subset by columns
#' and not by rows) the \code{V1.name} and \code{V2.name} parameters can be used 
#' to specify a vector of the same length
#' as the data frame to be subsetted in each study in which every element is 1 and 
#' there are no missing values. For more information see the example 2 below. 
#' 
#' Server functions called: \code{dataFrameSubsetDS1} and \code{dataFrameSubsetDS2}
#' 
#' @param df.name a character string providing the name of the data frame to be subset. 
#' @param V1.name  A character string specifying the name of the vector 
#' to which the Boolean operator is to be applied to define the subset.
#' For more information see details. 
#' @param V2.name A character string specifying the name of the vector to compare 
#' with \code{V1.name}.
#' @param Boolean.operator A character string specifying one of six possible Boolean operators:
#' \code{'==', '!=', '>', '>=', '<'} and \code{'<='}. 
#' @param keep.cols a numeric vector specifying the numbers of the columns to be kept in the
#' final subset.
#' @param rm.cols a numeric vector specifying the numbers of the columns to be removed from 
#' the final subset.
#' @param keep.NAs logical, if TRUE the missing values are included in the subset. 
#' If FALSE or NULL all rows with at least one missing values are removed from the subset. 
#' @param newobj a character string that provides the name for the output 
#' object that is stored on the data servers. Default \code{dataframesubset.newobj}.
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} objects obtained after login.
#' If the \code{datasources}
#' the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#' @param notify.of.progress specifies if console output should be produced to indicate
#' progress. Default FALSE.
#' @return \code{ds.dataFrameSubset} returns
#' the object specified by the \code{newobj} argument
#' which is written to the server-side. 
#' Also, two validity messages are returned to the client-side indicating
#' the name of the \code{newobj} which has been created in each data source
#'  and if it is in a valid form.
#' @examples 
#' \dontrun{
#' 
#'  ## Version 6, for version 5 see the Wiki
#'   
#'   # connecting to the Opal servers
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
#'   connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D") 
#'   
#'   # Subsetting a data frame
#'   #Example 1: Include some rows and all columns in the subset
#'   ds.dataFrameSubset(df.name = "D",
#'                      V1.name = "D$LAB_TSC",
#'                      V2.name = "D$LAB_TRIG",
#'                      Boolean.operator = ">",
#'                      keep.cols = NULL, #All columns are included in the new subset
#'                      rm.cols = NULL, #All columns are included in the new subset
#'                      keep.NAs = FALSE, #All rows with NAs are removed
#'                      newobj = "new.subset",
#'                      datasources = connections[1],#only the first server is used ("study1")
#'                      notify.of.progress = FALSE)
#'   #Example 2: Include all rows and some columns in the new subset
#'     #Select complete cases (rows without NA)
#'     ds.completeCases(x1 = "D",
#'                      newobj = "complet",
#'                      datasources = connections)
#'     #Create a vector with all ones
#'     ds.make(toAssign = "complet$LAB_TSC-complet$LAB_TSC+1",
#'             newobj = "ONES",
#'             datasources = connections) 
#'     #Subset the data
#'     ds.dataFrameSubset(df.name = "complet",
#'                        V1.name = "ONES",
#'                        V2.name = "ONES",
#'                        Boolean.operator = "==",
#'                        keep.cols = c(1:4,10), #only columns 1, 2, 3, 4 and 10 are selected
#'                        rm.cols = NULL,
#'                        keep.NAs = FALSE,
#'                        newobj = "subset.all.rows",
#'                        datasources = connections, #all servers are used
#'                        notify.of.progress = FALSE)                
#'                      
#'   # Clear the Datashield R sessions and logout                 
#'   datashield.logout(connections) 
#'   
#' }   
#' @author DataSHIELD Development Team
#' @export

ds.dataFrameSubset<-function(df.name=NULL, V1.name=NULL, V2.name=NULL, Boolean.operator=NULL, keep.cols=NULL, rm.cols=NULL, keep.NAs=NULL, newobj=NULL, datasources=NULL, notify.of.progress=FALSE){

  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }

  # check if user has provided the name of the data.frame to be subsetted
  if(is.null(df.name)){
    stop("Please provide the name of the data.frame to be subsetted as a character string: eg 'xxx'", call.=FALSE)
  }

  # check if user has provided the name of the column or scalar that holds V1. If not
  # have they instead provided a non.null keep.cols or non.null rm.cols argument
  # in either of the latter cases then one needs to select all rows so
  # specify V1.name = "ONES" and this will specify a vector of 1s on the serverside
  # of length equal to dim[1] of the data.frame	
 
  if(is.null(keep.cols)&&is.null(rm.cols)&&is.null(V1.name))
  {	
    stop("Please provide the name of the column or scalar that holds V1 as a character string: eg 'xxx' or '3'", call.=FALSE)
  }
 
  if((!is.null(keep.cols)||!is.null(rm.cols))&&is.null(V1.name))
  {	
     V1.name<-"ONES"
  }
 
  # check if user has provided the name of the column or scalar that holds V2. If not
  # have they instead provided a non.null keep.cols or non.null rm.cols argument
  # in either of the latter cases then one needs to select all rows so
  # specify V2.name = "ONES" and this will specify a vector of 1s on the serverside
  # of length equal to dim[1] of the data.frame	
 
  if(is.null(keep.cols)&&is.null(rm.cols)&&is.null(V2.name))
  {	
    stop("Please provide the name of the column or scalar that holds V2 as a character string: eg 'xxx' or '3'", call.=FALSE)
  }
 
  if((!is.null(keep.cols)||!is.null(rm.cols))&&is.null(V2.name))
  {	
    V2.name<-"ONES"
  }

  if(is.null(keep.cols)&&is.null(rm.cols)&&is.null(Boolean.operator))
  {
    message1<-"Unless you are only subsetting columns, please provide a" 
    message2<-"Boolean operator in character format: eg '==' or '>=' or '<' or '!='."
    message3<-"However, if either keep.cols or rm.cols is non-null because you want"
    message4<-"to subset columns and you specify both V1.name and V2.name as NULL (or 'ONES')"
    message5<-"and Boolean.operator as NULL,ds.dataFrameSubset will subset out" 
    message6<-"the specified columns but will keep all rows."
 
    error.message<-paste(message1,message2,message3,message4,message5,message6)
    stop(error.message, call.=FALSE)
  }
 
  if((!is.null(keep.cols)||!is.null(rm.cols))&&is.null(Boolean.operator))
  {	
    Boolean.operator<-"=="
  }

  #if keep.NAs is set as NULL convert to FALSE as otherwise the call to datashield.assign will fail
  if(is.null(keep.NAs)){
  keep.NAs<-FALSE
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
    newobj <- "dataframesubset.newobj"
  }

 if(!is.null(keep.cols)){
  keep.cols<-paste0(as.character(keep.cols),collapse=",")
 }

if(!is.null(rm.cols)){
  rm.cols<-paste0(as.character(rm.cols),collapse=",")
 }

    calltext1 <- call("dataFrameSubsetDS1", df.name, V1.name, V2.name, BO.n, keep.cols, rm.cols, keep.NAs)
    return.warning.message<-DSI::datashield.aggregate(datasources, calltext1)

    calltext2 <- call("dataFrameSubsetDS2", df.name, V1.name, V2.name, BO.n, keep.cols, rm.cols, keep.NAs)
    DSI::datashield.assign(datasources, newobj, calltext2)

    numsources<-length(datasources)
    for(s in 1:numsources){
	num.messages<-length(return.warning.message[[s]])
        if (notify.of.progress)
	{
            if(num.messages==1){
	        message("\nSource",s,"\n",return.warning.message[[s]][[1]],"\n")
	    }else{
	        message("\nSource",s,"\n")
		for(m in 1:(num.messages-1)){
		    message(return.warning.message[[s]][[m]],"\n")
                }
                message(return.warning.message[[s]][[num.messages]],"\n")
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
#ds.dataFrameSubset
