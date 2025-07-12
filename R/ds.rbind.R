#' @title Combines R objects by rows in the server-side
#' @description It takes a sequence of vector, matrix or data-frame arguments
#' and combines them by rows to produce a matrix.
#' @details A sequence of vector, matrix or data-frame arguments
#' is combined  by rows to produce a matrix on the server-side.
#' 
#' In \code{DataSHIELD.checks} the checks are relatively slow. 
#' Default \code{DataSHIELD.checks} value is FALSE. 
#' 
#' If \code{force.colnames} is NULL column names are inferred from the names or column names
#' of the first object specified in the \code{x} argument.
#' The vector of column names must have the same number of elements as 
#' the columns in the output object.
#' 
#' Server functions called: \code{rbindDS}. 
#' 
#' 
#' @param x a character vector with the  name of the objects to be combined.  
#' @param DataSHIELD.checks logical, if TRUE checks that all
#' input objects exist and are of an appropriate class. 
#' @param force.colnames can be NULL or a vector of characters that 
#' specifies column names of the output object. 
#' @param newobj a character string that provides the name for the output variable 
#' that is stored on the data servers. Defaults \code{rbind.newobj}. 
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} objects obtained after login. 
#' If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#' @param notify.of.progress specifies if console output should be produced to indicate
#' progress. Default FALSE.
#' @return \code{ds.rbind} returns a matrix combining the rows of the 
#' R objects specified in the function
#' which is written to the server-side. 
#' It also returns two messages to the client-side with the name of \code{newobj}
#' that has been created in each data source and \code{DataSHIELD.checks} result. 
#' @examples 
#' 
#' \dontrun{
#'   ## Version 6, for version 5 see the Wiki 
#'   
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
#'   #Combining R objects by rows 
#'    
#'                    
#'   ds.rbind(x = "D", #data frames in the server-side to be conbined 
#'                     #(see above the connection to the Opal servers) 
#'            DataSHIELD.checks = FALSE,
#'            force.colnames = NULL,
#'            newobj = "D.rbind", # name for the output object that is stored in the data servers
#'            datasources = connections, # All Opal servers are used 
#'                                       #(see above the connection to the Opal servers)
#'            notify.of.progress = FALSE)
#'            
#'   # Clear the Datashield R sessions and logout  
#'   datashield.logout(connections) 
#'   }
#' 
#' @author DataSHIELD Development Team
#' @export
#' 
ds.rbind<-function(x=NULL, DataSHIELD.checks=FALSE, force.colnames=NULL, newobj=NULL, 
                   datasources=NULL, notify.of.progress=FALSE){

  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }

  if(is.null(x)){
    stop("Please provide a vector of character strings holding the name of the input elements!", call.=FALSE)
  }


  if(DataSHIELD.checks){
    
    # check if the input object(s) is(are) defined in all the studies
    lapply(x, function(k){isDefined(datasources, obj=k)})

    # call the internal function that checks the input object(s) is(are) of the same legal class in all studies.
    for(i in 1:length(x)){
      typ <- checkClass(datasources, x[i])
      if(!('data.frame' %in% typ) & !('matrix' %in% typ) & !('factor' %in% typ) & !('character' %in% typ) & !('numeric' %in% typ) & !('integer' %in% typ) & !('logical' %in% typ)){
        stop(" Only objects of type 'data.frame', 'matrix', 'numeric', 'integer', 'character', 'factor' and 'logical' are allowed.", call.=FALSE)
      }
    }
  }
  
  # check newobj not actively declared as null
  if(is.null(newobj)){
    newobj <- "rbind.newobj"
  }

#CREATE THE VECTOR OF COLUMN NAMES
if(!is.null(force.colnames)){
colname.vector<-force.colnames
}else{

  colname.vector<-NULL
  class.vector<-NULL

for(j in 1:length(x))
{
testclass.var<-x[j]

calltext1<-call('classDS', testclass.var)
next.class <- DSI::datashield.aggregate(datasources, calltext1)
class.vector<-c(class.vector,next.class[[1]])
if (notify.of.progress)
    cat("\n",j," of ", length(x), " elements to combine in step 1 of 2\n")
}

for(j in 1:length(x))
{
test.df<-x[j]

if(class.vector[j]!="data.frame" && class.vector[j]!="matrix")
	{
	colname.vector<-c(colname.vector,test.df)
        if (notify.of.progress)
            cat("\n",j," of ", length(x), " elements to combine in step 2 of 2\n")
	}
else
	{
	  calltext2 <- call('colnamesDS', test.df)
    df.names <- DSI::datashield.aggregate(datasources, calltext2)
	 colname.vector<-c(colname.vector,df.names[[1]])
         if (notify.of.progress)
              cat("\n",j," of ", length(x), " elements to combine in step 2 of 2\n")
        }
}
if (notify.of.progress)
    cat("\nBoth steps completed\n")

#CHECK FOR DUPLICATE NAMES IN COLUMN NAME VECTOR AND ADD ".k" TO THE kth REPLICATE
num.duplicates<-rep(0,length(colname.vector))

if(length(colname.vector)==1){
num.duplicates<-0
}else{
if(length(colname.vector>=2))
{
for(j in length(colname.vector):2)
{
	for(k in (j-1):1)
	{
	if(colname.vector[j]==colname.vector[k])
		{
		num.duplicates[j]<-num.duplicates[j]+1
		}
	}
}
}
}
    num.duplicates.c <- as.character(num.duplicates)

    for(m in 1:length(colname.vector)){
      if(num.duplicates[m]!="0"){
        colname.vector[m] <- paste0(colname.vector[m],".",num.duplicates.c[m])
  	  }
    }
  }

  # prepare name vectors for transmission
  x.names.transmit <- paste(x, collapse=",")
  colnames.transmit <- paste(colname.vector, collapse=",")

  # call the server side function
	calltext <- call("rbindDS", x.names.transmit, colnames.transmit)
	DSI::datashield.assign(datasources, newobj, calltext)


#############################################################################################################
#DataSHIELD CLIENTSIDE MODULE: CHECK KEY DATA OBJECTS SUCCESSFULLY CREATED                                  #
																											#
#SET APPROPRIATE PARAMETERS FOR THIS PARTICULAR FUNCTION                                                 	#
test.obj.name<-newobj																					 	#
																											#
																											#
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
#ds.rbind
