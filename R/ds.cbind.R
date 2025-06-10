#' @title Combines R objects by columns in the server-side
#' @description Takes a sequence of vector, matrix or data-frame arguments
#' and combines them by column to produce a data-frame.
#' @details A sequence of vector, matrix or data-frame arguments
#' is combined column by column to produce a data-frame that is written to the server-side. 
#' 
#' This function is similar to the native R function \code{cbind}.
#' 
#' In \code{DataSHIELD.checks} the checks are relatively slow. 
#' Default \code{DataSHIELD.checks} value is FALSE.
#' 
#' If \code{force.colnames} is NULL (which is recommended), the column names are inferred
#' from the names or column names of the first object specified in the \code{x} argument.
#' If this argument is not NULL, then the column names of the assigned data.frame have the
#' same order as the characters specified by the user in this argument. Therefore, the
#' vector of \code{force.colnames} must have the same number of elements as the columns in
#' the output object. In a multi-site DataSHIELD setting to use this argument, the user should
#' make sure that each study has the same number of names and column names of the input elements
#' specified in the \code{x} argument and in the same order in all the studies. 
#' 
#' Server function called: \code{cbindDS}
#' 
#' @param x a character vector with the  name of the objects to be combined.
#' @param DataSHIELD.checks logical. if TRUE does four checks:\cr
#' 1. the input object(s) is(are) defined in all the studies.\cr
#' 2. the input object(s) is(are) of the same legal class in all the studies.\cr
#' 3. if there are any duplicated column names in the input objects in each study.\cr
#' 4. the number of rows is the same in all components to be cbind.\cr
#' Default FALSE. 
#' @param force.colnames can be NULL (recommended) or a vector of characters that specifies 
#' column names of the output object. If it is not NULL the user should take some caution. 
#' For more information see \strong{Details}.
#' @param newobj a character string that provides the name for the output variable 
#' that is stored on the data servers. Defaults \code{cbind.newobj}. 
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} objects obtained after login. 
#' If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#' @param notify.of.progress specifies if console output should be produced to indicate
#' progress. Default FALSE.
#' @return \code{ds.cbind} returns a data frame combining the columns of the R 
#' objects specified in the function which is written to the server-side. 
#' It also returns to the client-side two messages with the name of \code{newobj}
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
#'   # Example 1: Assign the exponent of a numeric variable at each server and cbind it 
#'   # to the data frame D
#'   
#'   ds.exp(x = "D$LAB_HDL",
#'          newobj = "LAB_HDL.exp",
#'          datasources = connections) 
#'          
#'   ds.cbind(x = c("D", "LAB_HDL.exp"),
#'            DataSHIELD.checks = FALSE,
#'            newobj = "D.cbind.1",
#'            datasources = connections)
#'              
#'   # Example 2: If there are duplicated column names in the input objects the function adds
#'   # a suffix '.k' to the kth replicate". If also the argument DataSHIELD.checks is set to TRUE
#'   # the function returns a warning message notifying the user for the existence of any duplicated
#'   # column names in each study
#'   
#'   ds.cbind(x = c("LAB_HDL.exp", "LAB_HDL.exp"), 
#'            DataSHIELD.checks = TRUE,
#'            newobj = "D.cbind.2",
#'            datasources = connections)
#'            
#'   ds.colnames(x = "D.cbind.2",
#'               datasources = connections)            
#'              
#'   # Example 3: Generate a random normally distributed variable of length 100 at each study,
#'   # and cbind it to the data frame D. This example fails and  returns an error as the length
#'   # of the generated variable "norm.var" is not the same as the number of rows in the data frame D
#'   
#'   ds.rNorm(samp.size = 100,
#'            newobj = "norm.var",
#'            datasources = connections) 
#'            
#'   ds.cbind(x = c("D", "norm.var"), 
#'            DataSHIELD.checks = FALSE,
#'            newobj = "D.cbind.3", 
#'            datasources = connections)                 
#'                    
#'   # Clear the Datashield R sessions and logout  
#'   datashield.logout(connections) 
#'   }
#' 
#' @author DataSHIELD Development Team
#' @export
#' 
ds.cbind <- function(x=NULL, DataSHIELD.checks=FALSE, force.colnames=NULL, newobj=NULL, datasources=NULL, notify.of.progress=FALSE){

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
        stop("Only objects of type 'data.frame', 'matrix', 'numeric', 'integer', 'character', 'factor' and 'logical' are allowed.", call.=FALSE)
      }
    }
    
    # check that there are no duplicated column names in the input components
    for(j in 1:length(datasources)){
      colNames <- list()
      for(i in 1:length(x)){
        typ <- checkClass(datasources, x[i])
        if(any(typ %in% c('data.frame', 'matrix'))){
          colNames[[i]] <- ds.colnames(x=x[i], datasources=datasources[j])
        }
        if(any(typ %in% c('factor', 'character', 'numeric', 'integer', 'logical'))){
          colNames[[i]] <- as.character(x[i])
        }
        colNames <- unlist(colNames)
        if(anyDuplicated(colNames) != 0){
          cat("\n Warning: Some column names in study", j, "are duplicated and a suffix '.k' will be added to the kth replicate \n")
        }  
      }  
    } 
    
    # check that the number of rows is the same in all componets to be cbind
    for(j in 1:length(datasources)){
      nrows <- list()
      for(i in 1:length(x)){
        typ <- checkClass(datasources, x[i])
        if(any(typ %in% c('data.frame', 'matrix'))){
          nrows[[i]] <- ds.dim(x=x[i], type='split', datasources=datasources[j])[[1]][1]
        }
        if(any(typ %in% c('factor', 'character', 'numeric', 'integer', 'logical'))){
          nrows[[i]] <- ds.length(x[i], type='split', datasources=datasources[j])[[1]]
        }
      }
      nrows <- unlist(nrows)
      if(any(nrows != nrows[1])){
        stop("The number of rows is not the same in all of the components to be cbind", call.=FALSE)
      }
    }  
    
  }
  
  # check newobj not actively declared as null
  if(is.null(newobj)){
    newobj <- "cbind.newobj"
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
        cat("\n",j," of ", length(x), " elements to combine in step 1 of 2 in study ", std, "\n")
      }  
    }
    for(j in 1:length(x)){
      test.df <- x[j]
      if(class.vector[j]!="data.frame" && class.vector[j]!="matrix"){
        colname.vector <- c(colname.vector, test.df)
        if (notify.of.progress){
          cat("\n",j," of ", length(x), " elements to combine in step 2 of 2 in study ", std, "\n")
        }  
      }else{
        calltext2 <- call('colnamesDS', test.df)
        df.names <- DSI::datashield.aggregate(datasources[std], calltext2)
        colname.vector <- c(colname.vector, df.names[[1]])
        if (notify.of.progress){
          cat("\n", j," of ", length(x), " elements to combine in step 2 of 2 in study ", std, "\n")
        }  
      }
    }
    colname.list[[std]] <- colname.vector
  }
    
  if (notify.of.progress){
    cat("\nBoth steps in all studies completed\n")
  }
    
  # prepare name vectors for transmission
  x.names.transmit <- paste(x, collapse=",")
  colnames.transmit <- list()
  for (std in 1:length(datasources)){
    colnames.transmit[[std]] <- paste(colname.list[[std]], collapse=",")
  }
  
  ###############################
  # call the server side function
  for(std in 1:length(datasources)){
    calltext <- call("cbindDS", x.names.transmit, colnames.transmit[[std]])
    DSI::datashield.assign(datasources[std], newobj, calltext)
  }
  
  #############################################################################################################
  # DataSHIELD CLIENTSIDE MODULE: CHECK KEY DATA OBJECTS SUCCESSFULLY CREATED  
  
  # SET APPROPRIATE PARAMETERS FOR THIS PARTICULAR FUNCTION 
  test.obj.name <- newobj	
  
  # CALL SEVERSIDE FUNCTION
  calltext <- call("testObjExistsDS", test.obj.name)
  object.info <- DSI::datashield.aggregate(datasources, calltext)

  # CHECK IN EACH SOURCE WHETHER OBJECT NAME EXISTS
  # AND WHETHER OBJECT PHYSICALLY EXISTS WITH A NON-NULL CLASS
  num.datasources <- length(object.info)
  
  obj.name.exists.in.all.sources <- TRUE
  obj.non.null.in.all.sources <- TRUE

  for(j in 1:num.datasources){
  	if(!object.info[[j]]$test.obj.exists){
  		obj.name.exists.in.all.sources <- FALSE
  	}
  	if(is.null(object.info[[j]]$test.obj.class) || ("ABSENT" %in% object.info[[j]]$test.obj.class)){
  		obj.non.null.in.all.sources <- FALSE
  	}
  }

  if(obj.name.exists.in.all.sources && obj.non.null.in.all.sources){
		return.message <- paste0("A data object <", test.obj.name, "> has been created in all specified data sources")
	}else{
    return.message.1 <- paste0("Error: A valid data object <", test.obj.name, "> does NOT exist in ALL specified data sources")
  	return.message.2 <- paste0("It is either ABSENT and/or has no valid content/class,see return.info above")	
  	return.message.3 <-	paste0("Please use ds.ls() to identify where missing")
  	return.message <- list(return.message.1,return.message.2,return.message.3)
	}

	calltext <- call("messageDS", test.obj.name)
  studyside.message <- DSI::datashield.aggregate(datasources, calltext)
	no.errors <- TRUE
	for(nd in 1:num.datasources){
		if(studyside.message[[nd]]!="ALL OK: there are no studysideMessage(s) on this datasource"){
		no.errors <- FALSE
		}
	}
	
	if(no.errors){
	  validity.check <- paste0("<",test.obj.name, "> appears valid in all sources")
	  return(list(is.object.created=return.message,validity.check=validity.check))
	}

  if(!no.errors){
	  validity.check <- paste0("<",test.obj.name,"> invalid in at least one source. See studyside.messages:")
	  return(list(is.object.created=return.message,validity.check=validity.check,
	            studyside.messages=studyside.message))
  }
	
  # END OF CHECK OBJECT CREATED CORECTLY MODULE	
  #######################################################################################################

}
#ds.cbind
