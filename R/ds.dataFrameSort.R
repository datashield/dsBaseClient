#'
#' @title Sorts data frames in the server-side
#' @description Sorts a data frame using a specified sort key.
#' @details It sorts a specified
#' data.frame on the serverside using a sort key also on the server-side. The
#' sort key can either sit in the data.frame or outside it. 
#' The sort key can be forced to be interpreted as alphabetic or numeric. 
#' 
#' When a numeric vector is sorted alphabetically, the order can look confusing. 
#' For example, if we have a numeric vector to sort:\cr 
#' vector.2.sort = c(-192, 76, 841, NA, 1670, 163, 147, 101, -112, -231, -9, 119, 112, NA)\cr
#' 
#' When sorting numbers in an ascending (default) manner,
#' the largest negative numbers get ordered first 
#' leading up to the largest positive numbers
#' and finally (by default in R) NAs being positioned at the end of the vector:\cr
#' numeric.sort = c(-231, -192, -112, -9, 76, 101, 112, 119, 147, 163, 841, 1670, NA, NA)\cr
#' 
#' Instead, if the same vector is sorted alphabetically the the resultant vector is: 
#'
#' alphabetic.sort = (-112, -192, -231, -9, 101, 112, 119, 147, 163, 1670, 76, 841, NA, NA)\cr
#' 
#' Server function called: \code{dataFrameSortDS}. 
#' 
#' @param df.name a character string providing the name of the data frame
#' to be sorted.
#' @param sort.key.name a character string providing the name for the sort key.
#' @param sort.descending logical, if TRUE the data frame will be sorted.
#' by the sort key in descending order. Default = FALSE (sort order ascending).
#' @param sort.method a character string that specifies the method to be used 
#' to sort the data frame. This can be set as 
#' \code{"alphabetic"},\code{"a"} or \code{"numeric"}, \code{"n"}. 
#' @param newobj a character string that provides the name for the output data frame 
#' that is stored on the data servers. Default \code{dataframesort.newobj}.   
#' where \code{df.name} is the first argument of \code{ds.dataFrameSort()}.
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} 
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#' @return \code{ds.dataFrameSort} returns the sorted data frame is written to the server-side. 
#' Also, two validity messages are returned to the client-side
#' indicating the name of the \code{newobj} which 
#' has been created in each data source and if 
#' it is in a valid form.
#' @examples 
#' \dontrun{
#'   ## Version 6, for version 5 see the Wiki
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
#'   # Sorting the data frame
#'   ds.dataFrameSort(df.name = "D",
#'                    sort.key.name = "D$LAB_TSC",
#'                    sort.descending = TRUE,
#'                    sort.method = "numeric",
#'                    newobj = "df.sort",
#'                    datasources = connections[1]) #only the first Opal server is used ("study1")
#'                    
#'   # Clear the Datashield R sessions and logout                 
#'   datashield.logout(connections) 
#'   
#' }   
#' @author DataSHIELD Development Team
#' @export
#' 
ds.dataFrameSort<-function(df.name=NULL, sort.key.name=NULL, sort.descending=FALSE,
                           sort.method="default", newobj=NULL, datasources=NULL){
  
  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }

  if(is.null(sort.method)){
    sort.method <- "default"
  }
  
  if(sort.method!="numeric"&&sort.method!="n"&&sort.method!="alphabetic"&&sort.method!="a"){
    sort.method <- "default"
  }
  
  if(sort.method=="n"){
    sort.method <- "numeric"
  }
  
  if(sort.method=="a"){
    sort.method <- "alphabetic"
  }
  
  if(is.null(sort.descending)){
    sort.descending <- FALSE
  }
  
  if(is.null(newobj)){
    newobj <- "dataframesort.newobj"
  }
  
  # Call to assign function
  calltext <- call("dataFrameSortDS", df.name, sort.key.name, sort.descending, sort.method)
  datashield.assign(datasources, newobj, calltext)
  
  ###########################################################################################################
  #DataSHIELD CLIENTSIDE MODULE: CHECK KEY DATA OBJECTS SUCCESSFULLY CREATED                                #
  #																											#
  #SET APPROPRIATE PARAMETERS FOR THIS PARTICULAR FUNCTION                                                 	#
  test.obj.name<-newobj																					 	#
  #																											#
  #																											#
  # CALL SEVERSIDE FUNCTION                                                                                	#
  calltext <- call("testObjExistsDS", test.obj.name)													 	#
  #																											#
  #  object.info<-opal::datashield.aggregate(datasources, calltext)											#
  object.info<-datashield.aggregate(datasources, calltext)												 	#
  #																											#
  # CHECK IN EACH SOURCE WHETHER OBJECT NAME EXISTS														 	#
  # AND WHETHER OBJECT PHYSICALLY EXISTS WITH A NON-NULL CLASS											 	#
  num.datasources<-length(object.info)																	 	#
  #																											#
  #																											#
  obj.name.exists.in.all.sources<-TRUE																	 	#
  obj.non.null.in.all.sources<-TRUE																		 	#
  #																											#
  for(j in 1:num.datasources){																			 	#
    if(!object.info[[j]]$test.obj.exists){																 	#
      obj.name.exists.in.all.sources<-FALSE															 		#
    }																								 		#
    if(is.null(object.info[[j]]$test.obj.class) || ("ABSENT" %in% object.info[[j]]$test.obj.class)){				#														 	#
      obj.non.null.in.all.sources<-FALSE																 	#
    }																								 		#
  }																									 		#
  #																											#
  if(obj.name.exists.in.all.sources && obj.non.null.in.all.sources){										#
    #																											#
    return.message<-																					 	#
      paste0("A data object <", test.obj.name, "> has been created in all specified data sources")		 	#
    #																											#
    #																											#
  }else{																								 	#
    #																											#
    return.message.1<-																					 	#
      paste0("Error: A valid data object <", test.obj.name, "> does NOT exist in ALL specified data sources")	#
    #																											#
    return.message.2<-																					 	#
      paste0("It is either ABSENT and/or has no valid content/class,see return.info above")				 	#
    #																											#
    return.message.3<-																					 	#
      paste0("Please use ds.ls() to identify where missing")												#
    #																											#
    #																											#
    return.message<-list(return.message.1,return.message.2,return.message.3)							 	#
    #																											#
  }																											#
  #																											#
  calltext <- call("messageDS", test.obj.name)																#
  # studyside.message<-opal::datashield.aggregate(datasources, calltext)										#
  studyside.message<-datashield.aggregate(datasources, calltext)											#
  #																											#
  no.errors<-TRUE																							#
  for(nd in 1:num.datasources){																				#
    if(studyside.message[[nd]]!="ALL OK: there are no studysideMessage(s) on this datasource"){				#
      no.errors<-FALSE																						#
    }																										#
  }																											#	
  #																											#
  #																											#
  if(no.errors){																							#
    validity.check<-paste0("<",test.obj.name, "> appears valid in all sources")							    #
    return(list(is.object.created=return.message,validity.check=validity.check,					    		#
                studyside.messages=studyside.message))														#
  }																											#
  #
  if(!no.errors){																							#
    validity.check<-paste0("<",test.obj.name,"> invalid in at least one source. See studyside.messages:")   #
    return(list(is.object.created=return.message,validity.check=validity.check,					    		#
                studyside.messages=studyside.message))			                                            #
  }																											#
  #
  #END OF CHECK OBJECT CREATED CORRECTLY MODULE															 	#
  ###########################################################################################################
}
#ds.dataFrameSort
