#'
#' @title Gets the metadata associated with a variable held on the server
#' @description This function gets the metadata of a variable
#' stored on the server.
#' @details 
#' Server function \code{metadataDS} is called examines the attributes associated with the variable
#' which are non-disclosive.
#' @param x a character string specifying the name of the object.
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} 
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#' @return \code{ds.metadata} returns to the client-side the metadata of associated to an object
#' held at the server.
#' @author Stuart Wheater, DataSHIELD Development Team
#' @export
#' @examples
#' \dontrun{
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
#'   # Example 1: Get the metadata associated with variable 'D'
#'   ds.metadata(x = 'D$LAB_TSC', datasources = connections)
#'
#'   # clear the Datashield R sessions and logout
#'   DSI::datashield.logout(connections)
#' }
#'
ds.metadata = function(x=NULL, datasources=NULL)
{

    #####################################################################################
    #MODULE 1: IDENTIFY DEFAULT CONNECTIONS                                             #
    # look for DS connections                                                           #
    if (is.null(datasources)){                                                          #
        datasources <- datashield.connections_find()                                    #
    }                                                                                   #
    #####################################################################################

    ###############################################################################################################
    #MODULE 2: ENSURE CORRECT DATASOURCES                                                                         #
    # ensure datasources is a list of DSConnection-class                                                          #
    if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){ #
      stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)             #
    }                                                                                                             #
    ###############################################################################################################

    #####################################################################################
    #MODULE 3: SET UP KEY VARIABLES ALLOWING FOR DIFFERENT INPUT FORMATS                #
    if (is.null(x)){                                                                    #
        stop("Please provide the name of the object!", call.=FALSE)                     #
    }                                                                                   #
    #####################################################################################

    #####################################################################################
    #MODULE 4: CHECK CONSISTANCY OF VARIABLE NAME (SINGLE)                              #
    if ((! is.character(x)) || (length(x) != 1)){                                       #
        stop("Please provide the name of the object!", call.=FALSE)                     #
    }                                                                                   #
    #####################################################################################

    #####################################################################################
    #MODULE 5: CHECK ALL SERVICES HAVE SPECIFIED VARIABLES DEFINED                      #
    defined = all(unlist(isDefined(datasources, x)))                                    #
    if (! defined){                                                                     #
        stop("Variable not defined in all servers", call.=FALSE)                        #
    }                                                                                   #
    #####################################################################################

    cally     <- call("metadataDS", x)
    metadatas <- DSI::datashield.aggregate(datasources, cally)

    return(metadatas)

}
#ds.metadata
