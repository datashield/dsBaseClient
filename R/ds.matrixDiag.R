#' @title Calculates matrix diagonals in the server-side
#' @description Extracts the diagonal vector from a square matrix  or
#' creates a diagonal matrix  based on a vector or a scalar value 
#' on the server-side. 
#' @details The function behaviour is different depending on the 
#' value specified in the \code{aim} argument:\cr
#' (1) \code{If aim = "serverside.vector.2.matrix"}
#' the function takes a server-side vector and writes out a square matrix with
#' the vector as its diagonal and all off-diagonal \code{values = 0}. The dimensions
#' of the output matrix are determined by the length of the vector.
#' If the vector length is \code{k}, the output matrix has \code{k} rows and \code{k} columns.\cr
#' (2) If \code{aim = "serverside.scalar.2.matrix"}
#' the function takes a server-side scalar and writes out a square matrix with all
#' diagonal values equal to the value of the scalar
#' and all off-diagonal \code{values = 0}. The dimensions of the square
#' matrix are determined by the value of the \code{nrows.scalar} argument.\cr
#' (3) If \code{aim = "serverside.matrix.2.vector"}
#' the function takes a square server-side matrix and extracts
#' its diagonal values as a vector which is written to the server-side.\cr
#' (4) If \code{aim = "clientside.vector.2.matrix"}
#' the function takes a vector specified on the client-side
#' and writes out a square matrix to the server-side with
#' the vector as its diagonal and all off-diagonal \code{values = 0}. The dimensions
#' of the output matrix are determined by the length of the vector.\cr
#' (5) If \code{aim = "clientside.scalar.2.matrix"}
#' the function takes a scalar specified on the client-side
#' and writes out a square matrix with all diagonal values equal
#' to the value of the scalar. The dimensions of the square
#' matrix are determined by the value of the \code{nrows.scalar} argument.
#' 
#' If \code{x1} is a vector and the \code{nrows.scalar} 
#' is set as \code{k}, the vector will be used
#' repeatedly to fill up the diagonal. For example, the vector is of length
#' 7 and \code{nrows.scalar = 18}, a square diagonal matrix with
#' 18 rows and 18 columns will be created. 
#' 
#' Server function called: \code{matrixDiagDS}
#' @param x1 a character string specifying
#' the name of a server-side scalar or vector. Also, a numeric value or vector
#' specified from the client-side can be specified. This argument depends 
#' on the value specified in \code{aim}.
#' For more information see \strong{Details}. 
#' @param aim a character string specifying the behaviour of the function.
#' This can be set as: 
#' \code{"serverside.vector.2.matrix"}, \code{"serverside.scalar.2.matrix"},
#' \code{"serverside.matrix.2.vector"}, \code{"clientside.vector.2.matrix"}
#' or \code{"clientside.scalar.2.matrix"}.
#' For more information see \strong{Details}. 
#' @param nrows.scalar an integer specifying the dimensions of the matrix 
#' note that the matrix is square (same number of rows and columns).  
#' If this argument is not specified the matrix dimensions are 
#' defined by the length of the vector. 
#' For more information see \strong{Details}. 
#' @param newobj a character string that provides the name for the output 
#' variable that is stored on the data servers. Default \code{matrixdiag.newobj}.
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} 
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#' @return \code{ds.matrixDiag} returns to the server-side the square matrix diagonal. 
#' Also, two validity messages are returned
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
#'   #Example 1: Create a square matrix with the server-side vector as its diagonal
#'   #and all the other values = 0
#'   
#'   # Create a vector in the server-side 
#'   
#'   ds.rUnif(samp.size = 9,
#'            min = -10.5,
#'            max = 10.5,
#'            newobj = "ss.vector.9",
#'            seed.as.integer = 5575,
#'            force.output.to.k.decimal.places = 0,
#'            datasources = connections)
#'    
#'             
#'   #Calculate the diagonal of the matrix
#'   
#'   ds.matrixDiag(x1 = "ss.vector.9",
#'                 aim = "serverside.vector.2.matrix",
#'                 nrows.scalar = NULL,
#'                 newobj = "matrix.diag1",
#'                 datasources = connections)
#'                 
#'   #Example 2: Create a square matrix with the server-side scalar as all diagonal values 
#'   and all the other values = 0
#'   
#'   #Create a scalar in the server-side
#'   
#'   ds.rUnif(samp.size = 1,
#'            min = -10.5,
#'            max = 10.5,
#'            newobj = "ss.scalar",
#'            seed.as.integer = 5575,
#'            force.output.to.k.decimal.places = 0,
#'            datasources = connections)
#'            
#'   #Calculate the diagonal of the matrix
#'            
#'  ds.matrixDiag(x1 = "ss.scalar",
#'                aim = "serverside.scalar.2.matrix",
#'                nrows.scalar = 4,
#'                newobj = "matrix.diag2",
#'                datasources = connections)
#'                
#'  #Example 3: Create a vector that contains the server-side matrix diagonal values
#'  
#'  #Create a matrix in the server-side
#'  
#'  ds.matrix(mdata = 10,
#'            from = "clientside.scalar",
#'            nrows.scalar = 3,
#'            ncols.scalar = 8,
#'            newobj = "ss.matrix",
#'            datasources = connections)
#'            
#'  #Extract the diagonal of the matrix
#'      
#'  ds.matrixDiag(x1 = "ss.matrix",
#'                aim = "serverside.matrix.2.vector",
#'                nrows.scalar = NULL,
#'                newobj = "vector.diag3",
#'                datasources = connections)
#'                    
#'  #Example 4: Create a square matrix with the client-side vector as a diagonal
#'  and all the other values = 0
#'  
#'  ds.matrixDiag(x1 = c(2,6,9,10),
#'                aim = "clientside.vector.2.matrix",
#'                nrows.scalar = NULL,
#'                newobj = "matrix.diag4",
#'                datasources = connections)
#'                
#'  #Example 5: Create a square matrix with the client-side scalar as all diagonal values 
#'  and all the other values = 0
#'  
#'  ds.matrixDiag(x1 = 4,
#'                aim = "clientside.scalar.2.matrix",
#'                nrows.scalar = 5,
#'                newobj = "matrix.diag5",
#'                datasources = connections)
#'   
#'   
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(connections)
#' }
#' @export
#'
ds.matrixDiag<-function(x1=NULL, aim=NULL, nrows.scalar=NULL, newobj=NULL, datasources=NULL){

  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }

  # check if a value has been provided for x1
  if(is.null(x1)){
    return("Error: x1 must have a value which is a character string, a numeric vector or a scalar")
  }

  # if no value spcified for output object, then specify a default
  if(is.null(newobj))
  {
    newobj <- "matrixdiag.newobj"
  }

  #process x1 to make transmittable depending on aim
  #Check that valid aim has been specified
  if(aim!="serverside.vector.2.matrix"&&aim!="serverside.scalar.2.matrix"&&aim!="serverside.matrix.2.vector"&&
      aim!="clientside.vector.2.matrix"&&aim!="clientside.scalar.2.matrix")
  {
  cat("            FAILED: aim must be specified as one of the following - 'serverside.vector.2.matrix',
        'serverside.scalar.2.matrix', 'serverside.matrix.2.vector',
        'clientside.vector.2.matrix', 'clientside.scalar.2.matrix'\n\n")
  return('Please respecify')
  }

  if(aim=="serverside.vector.2.matrix"||aim=="serverside.scalar.2.matrix"||aim=="serverside.matrix.2.vector")
  {
  x1.transmit<-x1
  }


  if(aim=="clientside.vector.2.matrix"||aim=="clientside.scalar.2.matrix")
  {
  x1.transmit<-paste0(as.character(x1),collapse=",")
  }

  #process <nrows> to make transmittable depending on aim
  #in principle a valid value of <nrows> cannot be negative

  if(is.null(nrows.scalar))
  {
  nrows.scalar<-c(-9)
  }

  nrows.transmit<-paste0(as.character(nrows.scalar),collapse=",")

# CALL THE MAIN SERVER SIDE FUNCTION
  calltext <- call("matrixDiagDS", x1.transmit, aim, nrows.transmit)
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
#ds.matrixDiag
