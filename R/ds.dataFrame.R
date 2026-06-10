#' @title Generates a data frame object in the server-side 
#' @description Creates a data frame from its elemental components:
#'  pre-existing data frames, single variables or matrices.
#' @details It creates a data frame by combining
#' pre-existing data frames, matrices or variables.
#' 
#' The length of all component variables and the number of rows 
#' of the  data frames or matrices must be the same.  The output 
#' data frame will have the same number of rows. 
#' 
#' Server functions called: \code{classDS}, \code{colnamesDS}, \code{dataFrameDS}
#' 
#' @param x a character string that provides the name of the objects
#' to be combined.
#' @param row.names	NULL, integer or character string that provides the
#'  row names of the output data frame.
#' @param check.rows logical. If TRUE then the rows are checked for consistency of
#' length and names. Default is FALSE. 
#' @param check.names logical. If TRUE the column names 
#' in the data frame are checked to ensure that is unique. Default is TRUE. 
#' @param stringsAsFactors logical. If true the character vectors are
#' converted to factors. Default TRUE.
#' @param completeCases logical. If TRUE rows with one or more 
#' missing values will be deleted from the output data frame.
#' Default is FALSE.
#' @param DataSHIELD.checks logical. Default FALSE. If TRUE undertakes all DataSHIELD checks 
#' (time-consuming) which are:\cr
#' 1. the input object(s) is(are) defined in all the studies\cr
#' 2. the input object(s) is(are) of the same legal class in all the studies\cr
#' 3. if there are any duplicated column names in the input objects in each study\cr
#' 4. the number of rows of the  data frames or matrices and the length of all component variables
#' are the same
#' @param newobj a character string that provides the name for the output data frame  
#' that is stored on the data servers. Default \code{dataframe.newobj}. 
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} objects obtained after login. 
#' If the \code{datasources} argument is not specified 
#' the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}. 
#' @param notify.of.progress specifies if console output should be produced to indicate
#' progress. Default is FALSE.
#' @return \code{ds.dataFrame} returns the object specified by the \code{newobj} argument
#' which is written to the serverside. Also, two validity messages are returned to the
#' client-side indicating the name of the \code{newobj} that has been created in each data source
#' and if it is in a valid form.
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
#'                  
#'   logindata <- builder$build()
#'   
#'   # Log onto the remote Opal training servers
#'   connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D") 
#'   
#'   # Create a new data frame
#'   ds.dataFrame(x = c("D$LAB_TSC","D$GENDER","D$PM_BMI_CATEGORICAL"),
#'                row.names = NULL,
#'                check.rows = FALSE,
#'                check.names = TRUE,
#'                stringsAsFactors = TRUE, #character variables are converted to a factor 
#'                completeCases = TRUE, #only rows with not missing values are selected
#'                DataSHIELD.checks = FALSE,
#'                newobj = "df1",
#'                datasources = connections[1], #only the first Opal server is used ("study1")
#'                notify.of.progress = FALSE)
#'
#'
#'   # Clear the Datashield R sessions and logout
#'   datashield.logout(connections) 
#' }
#' @author DataSHIELD Development Team
#' @export
ds.dataFrame <- function(x=NULL, row.names=NULL, check.rows=FALSE, check.names=TRUE, stringsAsFactors=TRUE, completeCases=FALSE, DataSHIELD.checks=FALSE, newobj=NULL, datasources=NULL, notify.of.progress=FALSE){

  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }

  if(is.null(x)){
    stop("Please provide the name of the list that holds the input vectors!", call.=FALSE)
  }

  # create a name by default if user did not provide a name for the new variable
  if(is.null(newobj)){
    newobj <- "dataframe.newobj"
  }

  if(DataSHIELD.checks){
    
    # check if the input object(s) is(are) defined in all the studies
    lapply(x, function(k){isDefined(datasources, obj=k)})
    
    # call the internal function that checks the input object(s) is(are) of the same legal class in all studies.
    for(i in 1:length(x)){
      typ <- checkClass(datasources, x[i])
      if(!('data.frame' %in% typ) & !('matrix' %in% typ) & !('factor' %in% typ) & !('character' %in% typ) & !('numeric' %in% typ) & !('integer' %in% typ) & !('logical' %in% typ)){
        stop("Only objects of type 'data.frame', 'matrix', 'numeric', 'integer', 'character', 'factor' and 'logical' are allowed.", call.=FALSE)
      }
    }
    
    # check that there are no duplicated column names in the input components
    for(j in 1:length(datasources)){
      colNames <- list()
      for(i in 1:length(x)){
        typ <- checkClass(datasources, x[i])
        if(typ %in% c('data.frame', 'matrix')){
          colNames[[i]] <- ds.colnames(x=x[i], datasources=datasources[j])
        }
        if(typ %in% c('factor', 'character', 'numeric', 'integer', 'logical')){
          colNames[[i]] <- as.character(x[i])
        }
        colNames <- unlist(colNames)
        if(anyDuplicated(colNames) != 0){
          message("\n Warning: Some column names in study", j, "are duplicated and a suffix '.k' will be added to the kth replicate \n")
        }  
      }  
    } 
    
    # check that the number of rows is the same in all componets to be cbind
    for(j in 1:length(datasources)){
      nrows <- list()
      for(i in 1:length(x)){
        typ <- checkClass(datasources, x[i])
        if(typ %in% c('data.frame', 'matrix')){
          nrows[[i]] <- ds.dim(x=x[i], type='split', datasources=datasources[j])[[1]][1]
        }
        if(typ %in% c('factor', 'character', 'numeric', 'integer', 'logical')){
          nrows[[i]] <- ds.length(x[i], type='split', datasources=datasources[j])[[1]]
        }
      }
      nrows <- unlist(nrows)
      if(any(nrows != nrows[1])){
        stop("The number of rows is not the same in all of the input components", call.=FALSE)
      }
    }  
    
  }
  
  # check newobj not actively declared as null
  if(is.null(newobj)){
    newobj <- "df_new"
  }

  # CREATE THE VECTOR OF COLUMN NAMES
  colname.list <- list()
  for (std in 1:length(datasources)){  
    colname.vector <- NULL
    class.vector <- NULL
    for(j in 1:length(x)){
      testclass.var <- x[j]
      calltext1 <- call('classDS', testclass.var)
      next.class <- DSI::datashield.aggregate(datasources[std], calltext1)
      class.vector <- c(class.vector, next.class[[1]])
      if (notify.of.progress){
        message("\n",j," of ", length(x), " elements to combine in step 1 of 2 in study ", std, "\n")
      }  
    }
    for(j in 1:length(x)){
      test.df <- x[j]
      if(class.vector[j]!="data.frame" && class.vector[j]!="matrix"){
        colname.vector <- c(colname.vector, test.df)
        if (notify.of.progress){
          message("\n",j," of ", length(x), " elements to combine in step 2 of 2 in study ", std, "\n")
        }  
      }else{
        calltext2 <- call('colnamesDS', test.df)
        df.names <- DSI::datashield.aggregate(datasources[std], calltext2)
        colname.vector <- c(colname.vector, df.names[[1]])
        if (notify.of.progress){
          message("\n", j," of ", length(x), " elements to combine in step 2 of 2 in study ", std, "\n")
        }  
      }
    }
    colname.list[[std]] <- colname.vector
  }
  
  if (notify.of.progress){
    message("\nBoth steps in all studies completed\n")
  }
  
  # prepare vectors for transmission
  x.names.transmit <- paste(x, collapse=",")
  colnames.transmit <- list()
  for (std in 1:length(datasources)){
    colnames.transmit[[std]] <- paste(colname.list[[std]], collapse=",")
  }
  
  if(!is.null(row.names)){
    row.names.transmit <- paste(row.names, collapse=",")
  }
  
 ###############################
  # call the server side function
  #The serverside function dataFrameDS calls dsBase::dataFrameDS in dsBase repository
  for(std in 1:length(datasources)){
    if(is.null(row.names)){
      cally <- call("dataFrameDS", x.names.transmit, NULL, check.rows, check.names,
                    colnames.transmit[[std]], stringsAsFactors, completeCases)
    }else{
      cally <- call("dataFrameDS", x.names.transmit, row.names.transmit, check.rows, check.names,
                    colnames.transmit[[std]], stringsAsFactors, completeCases)
    }
    DSI::datashield.assign(datasources[std], newobj, cally)
  }

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
#ds.dataFrame
