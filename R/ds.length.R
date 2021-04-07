#'
#' @title Gets the length of an object in the server-side
#' @description This function gets the length of a vector
#' or list that is stored on the server-side. 
#' This function is similar to the R function \code{length}.
#' @details 
#' Server function called: \code{lengthDS}
#' @param x a character string specifying the name of a vector or list. 
#' @param type a character that represents the type of analysis to carry out.
#' If \code{type} is set to \code{'combine'}, \code{'combined'}, \code{'combines'} or \code{'c'},
#'  a global length is returned
#' if \code{type} is set to \code{'split'}, \code{'splits'} or \code{'s'}, 
#' the length is returned separately for each study.
#' if \code{type} is set to \code{'both'} or \code{'b'}, 
#' both sets of outputs are produced. 
#' Default \code{'both'}. 
#' @param checks logical. If TRUE the model components are checked. 
#' Default FALSE to save time. It is suggested that checks
#' should only be undertaken once the function call has failed.
#' @param datasources a list of \code{\link{DSConnection-class}} 
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link{datashield.connections_default}}.
#' @return \code{ds.length} returns to the client-side the pooled length of a vector or a list, 
#' or the length of a vector or a list for each study separately.
#' @author DataSHIELD Development Team
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
#'   # Example 1: Get the total number of observations of the vector of
#'   # variable 'LAB_TSC' across all the studies
#'   ds.length(x = 'D$LAB_TSC', 
#'             type = 'combine',
#'             datasources = connections)
#'
#'   # Example 2: Get the number of observations of the vector of variable
#'   # 'LAB_TSC' for each study separately
#'   ds.length(x = 'D$LAB_TSC',
#'             type = 'split',
#'             datasources = connections)
#'
#'   # Example 3: Get the number of observations on each study and the total
#'   # number of observations across all the studies for the variable 'LAB_TSC'
#'   ds.length(x = 'D$LAB_TSC',
#'             type = 'both',
#'             datasources = connections)
#'   
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(connections)
#' }
#'
ds.length <- function(x=NULL, type='both', checks='FALSE', datasources=NULL){

  ##################################################################################################################
  #MODULE 1: IDENTIFY DEFAULT CONNECTIONS                                                                          #
  # look for DS connections                                                                                        #
  if(is.null(datasources)){                                                                                        #
    datasources <- datashield.connections_find()                                                                   #
  }                                                                                                                #
                                                                                                                   #
  # ensure datasources is a list of DSConnection-class                                                             #
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){    #
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)                #
  }                                                                                                                #
  ##################################################################################################################

  #####################################################################################
  #MODULE 2: SET UP KEY VARIABLES ALLOWING FOR DIFFERENT INPUT FORMATS                #
  if(is.null(x)){                                                                   #
    stop("Please provide the name of the input vector!", call.=FALSE)               #
  }                                                                                 #
  # the input variable might be given as a variable in a data frame (i.e. D$x)      #
  # or just as a vector not attached to a table (i.e. x)                            #
  # we have to make sure the function deals with each case                          #
  xnames <- extract(x)                                                              #
  varname <- xnames$elements                                                        #
  obj2lookfor <- xnames$holders                                                     #
  #####################################################################################


  ###############################################################################################
  #MODULE 3: GENERIC OPTIONAL CHECKS TO ENSURE CONSISTENT STRUCTURE OF KEY VARIABLES            #
  #IN DIFFERENT SOURCES                                                                         #
  # beginning of optional checks - the process stops and reports as soon as one               #
  #check fails                                                                                #
  #
  if(checks){                                                                                 #
    message(" -- Verifying the variables in the model")                                       #
    #
    # check if the input object(s) is(are) defined in all the studies                           #
    if(is.na(obj2lookfor)){                                                                     #
      defined <- isDefined(datasources, varname)                                                #
    }else{                                                                                      #
      defined <- isDefined(datasources, obj2lookfor)                                            #
    }                                                                                           #
    #
    # call the internal function that checks the input object is suitable in all studies        #
    typ <- checkClass(datasources, x)                                                      #
    # the input object must be a vector or a list
    if(!('character' %in% typ) & !('factor' %in% typ) & !('integer' %in% typ) & !('logical' %in% typ) & !('numeric' %in% typ) & !('list' %in% typ)){
      stop("The input object must be a character, factor, integer, logical or numeric vector or a list.", call.=FALSE)
    }                                                                                         #
  }                                                                                             #
  ###############################################################################################

  ###################################################################################################
  #MODULE 4: EXTEND "type" argument to include "both" and enable valid alisases                     #
  if(type == 'combine' | type == 'combined' | type == 'combines' | type == 'c') type <- 'combine'   #
  if(type == 'split' | type == 'splits' | type == 's') type <- 'split'                              #
  if(type == 'both' | type == 'b' ) type <- 'both'                                                  #
  if(type != 'combine' & type != 'split' & type != 'both')                                          #
    stop('Function argument "type" has to be either "both", "combine" or "split"', call.=FALSE)     #
                                                                                                    #
  #MODIFY FUNCTION CODE TO DEAL WITH ALL THREE TYPES                                                #
  ###################################################################################################

  cally <- call("lengthDS", x)
  lengths <- DSI::datashield.aggregate(datasources, cally)

  # names of the studies to be used in the output
  stdnames <- names(datasources)
  outputnames <- c()
  for (i in 1:length(datasources)){
    outputnames[i] <- paste0('length of ', x, ' in ', stdnames[i])
  }

  # calculate the combined length of the vector from all studies
  pooled.length <- list(sum(unlist(lengths)))

  if(type=="combine"){
    out <- pooled.length
    names(out) <- paste0('total length of ', x, ' in all studies combined')
  }else{
    if(type=="split"){
	  out <- lengths
      names(out) <- outputnames
    }else{
      if(type=="both"){
        out <- c(lengths, pooled.length)
        names(out) <- c(outputnames, paste0('total length of ', x, ' in all studies combined'))
      }else{
        stop('Function argument "type" has to be either "both", "combine" or "split"')
      }
    }
  }

  return(out)

}
#ds.length
