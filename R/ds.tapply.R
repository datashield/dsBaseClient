#' @title Applies a Function Over a Server-Side Ragged Array
#' @description Apply one of a selected range of functions to summarize an
#' outcome variable over one or more indexing factors. 
#' The resultant summary is written to the client-side. 
#' @details This function is similar to a native R function \code{tapply()}.
#' It applies one of a selected range of functions to each cell 
#' of a ragged array, that is to each (non-empty)
#' group of values given by each unique combination of a series of indexing factors. 
#' 
#' The range of allowable summarizing functions for DataSHIELD \code{ds.tapply} function
#' is much more restrictive than for the native R \code{tapply} function.
#' The reason for this is the protection against disclosure risk. 
#' 
#' 
#' Should other functions
#' be required in the future then, provided they are non-disclosive, the DataSHIELD development
#' team could work on them if requested.
#' 
#' To protect against disclosure the
#' number of observations in each summarizing group in each source is calculated
#' and if any of these falls below the value of \code{nfilter.tab} 
#' (the minimum allowable non-zero count
#' in a contingency table) the tapply analysis of that source will return only an error message.
#' The value of \code{nfilter.tab} is can be set and modified only by the data custodian. If an
#' analytic team wishes the value to be reduced (e.g. to 1 which will allow any output
#' from tapply to be returned) this needs to formally be discussed and agreed
#' with the data custodian. 
#' 
#' If the reason for the tapply analysis is, for example, to break
#' a dataset down into a small number of values for each individual and then to flag up
#' which individuals have got at least one positive value for a binary outcome variable, then
#' that flagging does not have to be overtly returned to the client-side. Rather, it can be
#' written as a vector to the server-side at each source (which, like any other server-side
#' object, cannot then be seen, abstracted or copied). This can be done using 
#' \code{ds.tapply.assign} which writes the results as a \code{newobj} to the server-side 
#' and does not test the number of observations in each group against \code{nfilter.tab}. 
#' For more information see the help option of \code{ds.tapply.assign} function. 
#' 
#' The native R
#' tapply function has optional arguments such as \code{na.rm = TRUE} for \code{FUN = mean}
#' which will exclude any NAs from the outcome variable to be summarized. 
#' However, in order to keep DataSHIELD's \code{ds.tapply} and \code{ds.tapply.assign} 
#' functions straightforward, the
#' server-side functions \code{tapplyDS} and \code{tapplyDS.assign} both starts by stripping
#' any observations which have missing (NA) values in either the outcome variable or in
#' any one of the indexing factors. In consequence, the resultant analyses are always based
#' on complete cases.
#' 
#' In \code{INDEX.names} argument the native R tapply function
#' can coerce non-factor vectors into factors. However, this does not always work when
#' using the DataSHIELD \code{ds.tapply} or \code{ds.tapply.assign} 
#' functions so if you are concerned that
#' an indexing vector is not being treated correctly as a factor,
#' please first declare it explicitly as a factor using \code{ds.asFactor}. 
#' 
#' In \code{FUN.name} argument the allowable functions are: N or length (the number
#' of (non-missing) observations in the group defined by each combination of indexing
#' factors); mean; SD (standard deviation); sum; quantile (with quantile probabilities set at
#' c(0.05,0.1,0.2,0.25,0.3,0.33,0.4,0.5,0.6,0.67,0.7,0.75,0.8,0.9,0.95)).
#' 
#' Server function called: \code{tapplyDS}
#' @param X.name a character string specifying the name of the variable to be summarized. 
#' @param INDEX.names a character string specifying the name of a single factor 
#' or a list or vector of names of up to two factors to index the variable to be summarized. 
#' For more information see \strong{Details}. 
#' @param FUN.name a character string specifying the name of one of the allowable 
#' summarizing functions. This can be set as: 
#' \code{"N"} (or \code{"length"}), \code{"mean"},\code{"sd"}, \code{"sum"},
#' or \code{"quantile"}. 
#' For more information see \strong{Details}.
#' @param datasources  a list of \code{\link[DSI]{DSConnection-class}} 
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#' @return \code{ds.tapply} returns to the client-side an array of the summarized values.
#' It has the same number of dimensions as INDEX.
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
#'   # Apply a Function Over a Server-Side Ragged Array
#'   
#'   ds.assign(toAssign = "D$LAB_TSC",
#'             newobj = "LAB_TSC",
#'             datasources = connections)
#'             
#'   ds.assign(toAssign = "D$GENDER",
#'             newobj =  "GENDER",
#'             datasources = connections)
#'             
#'   ds.tapply(X.name = "LAB_TSC",
#'             INDEX.names = c("GENDER"),
#'             FUN.name = "mean",
#'             datasources = connections)
#'                  
#'   # Clear the Datashield R sessions and logout                 
#'   datashield.logout(connections) 
#'   
#' } 
#' @author DataSHIELD Development Team
#' @export
ds.tapply <- function(X.name=NULL, INDEX.names=NULL, FUN.name=NULL, datasources=NULL){

  ###datasources
  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }

  ###X.name
  # check if user has provided the name of the column that holds X.name
  if(is.null(X.name)){
    return("Error: Please provide the name of the variable to be summarized, as a character string")
  }
  
  # check if the X object is defined in all the studies
  isDefined(datasources, X.name)

  ###INDEX.names
  # check if user has provided the name of the column(s) that holds INDEX.names
  if(is.null(INDEX.names)){
    Err.1 <- "Error: Please provide the name of the single factor or"
    Err.2 <- "the list of factors to index the variable to be summarized."
    Err.3 <- "In either case the argument must be specified in inverted commas"
    return(list(Error.message=Err.1, Err.cont2=Err.2, Err.cont3=Err.3))
  }
  
  # check if the vector or list of INDEX.names includes up to two names
  if(length(INDEX.names) > 2){
    stop("The 'INDEX.names' can include the names of up to two factors", call.=FALSE)
  }
  
  # check if the INDEX objects are defined in all the studies
  for(i in 1:length(INDEX.names)){
    isDefined(datasources, INDEX.names[i])
  }

  # make INDEX.names transmitable
  if(!is.null(INDEX.names)){
    INDEX.names.transmit <- paste(INDEX.names, collapse=",")
  }else{
    INDEX.names.transmit <- NULL
  }

  ###FUN.name
  # check if user has provided a valid summarizing function
  if(is.null(FUN.name)){
    return("Error: Please provide a valid summarizing function, as a character string")
  }

  # CALL THE PRIMARY SERVER SIDE FUNCTION
  calltext <- call("tapplyDS", X.name, INDEX.names.transmit, FUN.name)
  output <- DSI::datashield.aggregate(datasources, calltext)
  return(output)

}
#ds.tapply
