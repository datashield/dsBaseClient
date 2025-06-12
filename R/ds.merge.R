#' @title Merges two data frames in the server-side
#' @description Merges (links) two data frames together based on common
#' values in defined vectors in each data frame. 
#' @details This function is similar to the native R function \code{merge}.
#' There are some changes compared with the native R function 
#' in choosing which variables to use to merge the data frames, the function \code{merge}
#' is very flexible. For example, you can choose to merge
#' using all vectors that appear in both data frames. However, for \code{ds.merge}
#' in DataSHIELD it is required that all the vectors which dictate the merging
#' are explicitly identified for both data frames using the \code{by.x.names} and
#' \code{by.y.names} arguments. 
#' 
#' Server function called: \code{mergeDS}
#' @param x.name a character string specifying the name of the
#' first data frame to be merged. The length of the string should be less than the 
#' specified threshold for the nfilter.stringShort which is one of the disclosure 
#' prevention checks in DataSHIELD.
#' @param y.name a character string specifying the name of the
#' second data frame to be merged. The length of the string should be less than the 
#' specified threshold for the nfilter.stringShort which is one of the disclosure 
#' prevention checks in DataSHIELD.
#' @param by.x.names a character string  or a vector of names specifying 
#' of the column(s) in data frame \code{x.name} for merging. 
#' @param by.y.names a character string  or a vector of names specifying 
#' of the column(s) in data frame \code{y.name} for merging. 
#' @param all.x logical. If TRUE then extra rows will be added to the output,
#' one for each row in \code{x.name} that has no matching row in \code{y.name}. 
#' If FALSE the rows with data from both data frames are included in the output.
#' Default FALSE. 
#' @param all.y logical. If TRUE then extra rows will be added to the output,
#' one for each row in \code{y.name} that has no matching row in \code{x.name}. 
#' If FALSE the rows with data from both data frames are included in the output.
#' Default FALSE. 
#' @param sort logical. If TRUE the merged result is sorted on elements
#' in the \code{by.x.names} and \code{by.y.names} columns. Default TRUE.
#' @param suffixes a character vector of length 2 specifying the suffixes to be used for
#' making unique common column names in the two input data frames
#' when they both appear in the merged data frame. 
#' @param no.dups logical. Suffixes are appended in more cases to
#' avoid duplicated column names in the merged data frame. Default TRUE
#' (FALSE before R version 3.5.0). 
#' @param incomparables values that cannot be matched. This is intended to 
#' be used for merging on
#' one column, so these are incomparable values of that column.
#' For more information see \code{match} in native R \code{merge} function. 
#' @param newobj a character string that provides the name for the output 
#' variable that is stored on the data servers. Default \code{merge.newobj}. 
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} 
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#' @return \code{ds.merge} returns the merged data frame that is written on the server-side. 
#' Also, two validity messages are returned to the client-side
#' indicating whether the new object has been created in each data source and if so whether
#' it is in a valid form. 
#' @author DataSHIELD Development Team
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
#'   #Create two data frames with a common column
#'   
#'   ds.dataFrame(x = c("D$LAB_TSC","D$LAB_TRIG","D$LAB_HDL","D$LAB_GLUC_ADJUSTED"),
#'                completeCases = TRUE,
#'                newobj = "df.x",
#'                datasources = connections)
#'                
#'   ds.dataFrame(x = c("D$LAB_TSC","D$GENDER","D$PM_BMI_CATEGORICAL","D$PM_BMI_CONTINUOUS"),
#'                completeCases = TRUE,
#'                newobj = "df.y",
#'                datasources = connections) 
#'                
#'   # Merge data frames using the common variable "LAB_TSC"
#'                
#'   ds.merge(x.name = "df.x",
#'            y.name = "df.y",
#'            by.x.names = "df.x$LAB_TSC",
#'            by.y.names = "df.y$LAB_TSC",
#'            all.x = TRUE,
#'            all.y = TRUE,
#'            sort = TRUE,
#'            suffixes = c(".x", ".y"),
#'            no.dups = TRUE,
#'            newobj = "df.merge",
#'            datasources = connections)              
#'   
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(connections)
#' }
#'
#' @export
#' 
ds.merge <- function(x.name=NULL,y.name=NULL, by.x.names=NULL, by.y.names=NULL,all.x=FALSE,all.y=FALSE,
			 sort=TRUE, suffixes = c(".x",".y"), no.dups=TRUE, incomparables=NULL, newobj=NULL, datasources=NULL){

  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }

  # dataframe names
  if(is.null(x.name)){
    stop("Please provide the name (eg 'name1') of first dataframe to be merged (called x) ", call.=FALSE)
  }

  if(is.null(y.name)){
    stop("Please provide the name (eg 'name2') of second dataframe to be merged (called y) ", call.=FALSE)
  }
  
  # check if the input objects are defined in all the studies
  isDefined(datasources, x.name)
  isDefined(datasources, y.name)

  # names of columns to merge on (may be more than one)
  if(is.null(by.x.names)){
    stop("Please provide the names of columns in x dataframe on which to merge (eg c('id', 'time'))", call.=FALSE)
	}

  if(is.null(by.y.names)){
    stop("Please provide the names of columns in y dataframe on which to merge (eg c('id', 'time'))", call.=FALSE)
	}

	# make transmittable via parser
  by.x.names.transmit <- paste(by.x.names, collapse=",")
  by.y.names.transmit <- paste(by.y.names, collapse=",")

  # suffixes
  if(is.null(suffixes)){
    stop("Please provide the suffixes to append to disambiguate duplicate column names (default = c('.x','.y'))", call.=FALSE)
  }
  
	# make transmittable via parser
  suffixes.transmit <- paste(suffixes, collapse=",")

  # create a name by default if user did not provide a name for the new dataframe
  if(is.null(newobj)){
    newobj <- "merge.newobj"
  }

  # call the server side function
	calltext <- call("mergeDS", x.name, y.name, by.x.names.transmit, by.y.names.transmit, all.x, all.y,
			 sort, suffixes.transmit, no.dups, incomparables)
	DSI::datashield.assign(datasources, newobj, calltext)


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
# ds.merge
