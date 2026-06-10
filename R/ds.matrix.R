#' @title Creates a matrix on the server-side
#' @description Creates a matrix on the server-side 
#' with dimensions specified by \code{nrows.scalar} 
#' and \code{ncols.scalar} arguments and assigns the 
#' values of all its elements based on the \code{mdata} argument. 
#' @details This function is similar to the R native function \code{matrix()}. 
#' 
#' If in the \code{mdata} argument a vector is specified this should have 
#' the same length as the total number of elements
#' in the matrix. If this is not TRUE  the values in \code{mdata}
#' will be used repeatedly until all elements in the matrix are full.
#' If \code{mdata} argument is a scalar, all elements in the matrix will take that value.
#' 
#' 
#' In the \code{nrows.scalar} argument can be a character string specifying 
#' the name of a server-side scalar. For example,  
#' if a server-side scalar named \code{ss.scalar} exists and holds the value 23,
#' then by specifying \code{nrows.scalar = "ss.scalar"}, the matrix created will
#' have 23 rows. Also this argument can be 
#' a numeric value from the
#' client-side. The same rules are applied to \code{ncols.scalar} argument but in this 
#' case the column numbers are specified. 
#' In both arguments a zero, negative, NULL or missing value is not permitted. 
#'
#' 
#' Server function called: \code{matrixDS}
#' 
#' @param mdata a character string specifying
#' the name of a server-side scalar or vector. Also, a numeric value representing a
#' scalar specified from the client-side can be specified 
#' Zeros, negative values and NAs are all allowed.
#' For more information see \strong{Details}. 
#' @param from a character string specifying the source and nature of \code{mdata}.
#' This can be set as \code{"serverside.vector"}, \code{"serverside.scalar"}
#' or \code{"clientside.scalar"}. Default \code{"clientside.scalar"}. 
#' @param nrows.scalar an integer or a character string that specifies the number 
#' of rows in the matrix to be created.
#' For more information see \strong{Details}. 
#' @param ncols.scalar an integer or a character string that specifies 
#' the number of columns in the matrix to be created.
#' @param byrow logical. If TRUE and \code{mdata} is a vector the matrix
#' created should be filled row by row. If FALSE the matrix created should 
#' be filled column by column. Default = FALSE.
#' @param dimnames a list of length 2 giving
#' the row and column names respectively.
#' @param newobj a character string that provides the name for the output 
#' variable that is stored on the data servers. Default \code{matrix.newobj}. 
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} 
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#' @return \code{ds.matrix} returns the created matrix which is written on the server-side. 
#' In addition, two validity messages are returned
#' indicating whether the new matrix has been created in each data source and if so whether
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
#'   #Example 1: create a matrix with -13 value in all elements
#'   
#'   ds.matrix(mdata = -13,
#'             from = "clientside.scalar",
#'             nrows.scalar = 3,
#'             ncols.scalar = 8,
#'             newobj = "cs.block",
#'             datasources = connections)
#'             
#'   #Example 2: create a matrix of missing values 
#'
#'   ds.matrix(NA,
#'             from = "clientside.scalar",
#'             nrows.scalar = 4,
#'             ncols.scalar = 5,
#'             newobj = "cs.block.NA",
#'             datasources = connections)
#'
#'   #Example 3: create a matrix using a server-side vector
#'   #create a vector in the server-side
#'   
#'   ds.rUnif(samp.size = 45,
#'            min = -10.5,
#'            max = 10.5,
#'            newobj = "ss.vector",
#'            seed.as.integer = 8321,
#'            force.output.to.k.decimal.places = 0,
#'            datasources = connections)
#'            
#'   ds.matrix(mdata = "ss.vector",
#'             from = "serverside.vector",
#'             nrows.scalar = 5,
#'             ncols.scalar = 9,
#'             newobj = "sv.block",
#'             datasources = connections)
#'             
#'   #Example 4: create a matrix using a server-side vector and specifying
#'   #the row a column names
#'
#'   ds.rUnif(samp.size = 9,
#'            min = -10.5,
#'            max = 10.5,
#'            newobj = "ss.vector.9",
#'            seed.as.integer = 5575,
#'            force.output.to.k.decimal.places = 0,
#'            datasources = connections)
#'            
#'   ds.matrix(mdata = "ss.vector.9",
#'             from = "serverside.vector",
#'             nrows.scalar = 5,
#'             ncols.scalar = 9,
#'             byrow = TRUE,
#'             dimnames = list(c("a","b","c","d","e")),
#'             newobj = "sv.block.9.dimnames1",
#'             datasources = connections)
#'
#'   
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(connections)
#' }
#'
#' @export
#' 
ds.matrix <- function(mdata = NA, from="clientside.scalar", nrows.scalar=NULL, ncols.scalar=NULL, byrow = FALSE,
                     dimnames = NULL, newobj=NULL, datasources=NULL){

  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }

  # check if a value has been provided for mdata
  if(is.null(mdata)){
    return("Error: mdata must be a character string, a numeric vector or a scalar")
  }

  # if no value spcified for output object, then specify a default
  if(is.null(newobj)){
    newobj <- "matrix.newobj"
  }

  #process mdata to make transmittable depending on <from>
  #Check that valid from has been specified
  if(from!="serverside.vector"&&from!="serverside.scalar"&&from!="clientside.scalar")
  {
  message("            FAILED: <from> must be specified as one of the following - 'serverside.vector',
        'serverside.scalar', 'clientside.scalar'\n\n")
  return('Please respecify')
  }

  if(from=="serverside.vector"||from=="serverside.scalar"){
    mdata.transmit <- mdata
  }

  if(from=="clientside.scalar"){
    mdata.transmit <- paste0(as.character(mdata), collapse=",")
  }

  #process <nrows> and <ncols> to make transmittable depending on specified from
  #in principle acceptable values of <nrows> and <ncols> cannot be negative

  if(is.null(nrows.scalar))
  {
  nrows.scalar<-c(-9)
  }

  nrows.transmit<-paste0(as.character(nrows.scalar),collapse=",")

  if(is.null(ncols.scalar))
  {
  ncols.scalar<-c(-9)
  }
  ncols.transmit<-paste0(as.character(ncols.scalar),collapse=",")

# CALL THE MAIN SERVER SIDE FUNCTION
  calltext <- call("matrixDS", mdata.transmit, from, nrows.transmit, ncols.transmit, byrow, dimnames)
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
#ds.matrix
