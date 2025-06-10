#'
#' @title Retrieves the dimension of a server-side R object
#' @description Gives the dimensions of an R object on the server-side. 
#' This function is similar to R function \code{dim}. 
#' @details The function returns the dimension of the server-side 
#' input object (e.g. array, matrix or data frame)
#' from every single study and the pooled dimension of the object by summing up the individual 
#' dimensions returned from each study.
#' 
#' In \code{checks} parameter is suggested that checks should only be undertaken once the 
#' function call has failed.
#' 
#' Server function called: \code{dimDS}
#' 
#' @param x a character string providing the name of the input object. 
#' @param type a character string that represents the type of analysis to carry out. 
#' If \code{type} is set to \code{'combine'}, \code{'combined'}, \code{'combines'} or \code{'c'},
#'  the global dimension is returned. 
#' If \code{type} is set to \code{'split'}, \code{'splits'} or \code{'s'}, 
#' the dimension is returned separately for each study.
#' If \code{type} is set to \code{'both'} or \code{'b'}, both sets of outputs are produced.
#' Default \code{'both'}. 
#' @param checks logical. If TRUE undertakes all DataSHIELD checks (time-consuming).
#' Default FALSE.
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} 
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#' @return \code{ds.dim} retrieves to the client-side the dimension of the object 
#' in the form of a vector where the first
#' element indicates the number of rows and the second element indicates the number of columns.
#' @author DataSHIELD Development Team
#' @seealso \code{\link{ds.dataFrame}} to generate a table of the type data frame.
#' @seealso \code{\link{ds.changeRefGroup}} to change the reference level of a factor.
#' @seealso \code{\link{ds.colnames}} to obtain the column names of a matrix or a data frame
#' @seealso \code{\link{ds.asMatrix}} to coerce an object into a matrix type.
#' @seealso \code{\link{ds.length}} to obtain the size of a vector.
#' @export
#' @examples
#' \dontrun{
#'
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
#'
#'   # Calculate the dimension
#'   ds.dim(x="D", 
#'          type="combine", #global dimension
#'          checks = FALSE,
#'          datasources = connections)#all opal servers are used
#'   ds.dim(x="D",
#'          type = "both",#separate dimension for each study
#'                        #and the pooled dimension (default) 
#'          checks = FALSE,
#'          datasources = connections)#all opal servers are used
#'   ds.dim(x="D", 
#'          type="split", #separate dimension for each study
#'          checks = FALSE,
#'          datasources = connections[1])#only the first opal server is used ("study1")
#'
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(connections)
#'
#' }
#'
ds.dim <- function(x=NULL, type='both', checks=FALSE, datasources=NULL) {

  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }

  if(is.null(x)){
    stop("Please provide the name of a data.frame or matrix!", call.=FALSE)
  }

  ########################################################################################################
  # MODULE: GENERIC OPTIONAL CHECKS TO ENSURE CONSISTENT STRUCTURE OF KEY VARIABLES IN DIFFERENT SOURCES #
  # beginning of optional checks - the process stops and reports as soon as one check fails              #
  #                                                                                                      #
  if(checks){                                                                                            #
    message(" -- Verifying the variables in the model")                                                  #
    # check if the input object(s) is(are) defined in all the studies                                    #
    defined <- isDefined(datasources, x)                                                                 #                                                                                                #
    # call the internal function that checks the input object is suitable in all studies                 #
    typ <- checkClass(datasources, x)                                                                    #
    # throw a message and stop if input is not table structure                                           #
    if(!('data.frame' %in% typ) & !('matrix' %in% typ)){                                                 #
      stop("The input object must be a table structure!", call.=FALSE)                                   #
    }                                                                                                    #
  }                                                                                                      #
  ########################################################################################################


  ###################################################################################################
  #MODULE: EXTEND "type" argument to include "both" and enable valid aliases                        #
  if(type == 'combine' | type == 'combined' | type == 'combines' | type == 'c') type <- 'combine'   #
  if(type == 'split' | type == 'splits' | type == 's') type <- 'split'                              #
  if(type == 'both' | type == 'b' ) type <- 'both'                                                  #
  #
  #MODIFY FUNCTION CODE TO DEAL WITH ALL THREE TYPES                                                #
  ###################################################################################################

  cally <- call("dimDS", x)
  dimensions <- DSI::datashield.aggregate(datasources, cally)

  # names of the studies to be used in the output
  stdnames <- names(datasources)
  outputnames <- c()
  for (i in 1:length(datasources)){
    outputnames[i] <- paste0('dimensions of ', x, ' in ', stdnames[i])
  }

  # find the dimensions of the combined dataframe or matrix
  global.dim1 <- 0
  global.dim2 <- dimensions[[1]][2]
  for(i in 1:length(datasources)){
    global.dim1 <- global.dim1 + dimensions[[i]][1]
  }
  pooled.dim <- list(c(global.dim1, global.dim2))

  if(type=="combine"){
    out <- pooled.dim
	  names(out) <- paste0('dimensions of ', x, ' in combined studies')
  }else{
    if(type=="split"){
	  out <- dimensions
	  names(out) <- outputnames
    }else{
	    if(type=="both"){
        out <- c(dimensions, pooled.dim)
		    names(out) <- c(outputnames, paste0('dimensions of ', x, ' in combined studies'))
	    }else{
        stop('Function argument "type" has to be either "both", "combine" or "split"')
      }
    }
  }

  return(out)

}
#ds.dim
