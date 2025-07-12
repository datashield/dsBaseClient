#' @title Server-side random number generation
#' @description Primes the pseudorandom number generator in a data source
#' @details This function generates an instance of
#' the full pseudorandom number seed that is a vector of integers
#' of length 626 called \code{.Random.seed}, this vector is written to the server-side. 
#' 
#' This function is similar to a native R function \code{set.seed()}. 
#' 
#' In \code{seed.as.integer} argument 
#' the current limitation on the value of the integer that
#' can be specified is \code{-2147483647} up to \code{+2147483647} 
#' (this is \code{+/- ([2^31]-1)}).
#' 
#' Because you only specify one integer in the call to \code{ds.setSeed}
#' (i.e. the value for the \code{seed.as.integer} argument) that value will be
#' used as the priming trigger value in all of the specified
#' data sources and so the pseudorandom number generators will all start from
#' the same position and if a vector of pseudorandom number values is requested
#' based on one of DataSHIELD's pseudorandom number generating functions precisely
#' the same random vector will be generated in each source. If you want to avoid this
#' you can specify a different priming value in each source by using 
#' the \code{datasources} argument to generate the random number vectors one source
#' at a time with a different integer in each case. 
#' 
#' Furthermore, if you use any one
#' of DataSHIELD's pseudorandom number generating functions: \code{ds.rNorm}, \code{ds.rUnif},
#' \code{ds.rPois} or \code{ds.rBinom}. The function call itself automatically uses the single
#' integer priming seed you specify to generate different integers in each source.
#' 
#' 
#' Server function called: \code{setSeedDS}
#' @param seed.as.integer a numeric value or a NULL that primes the random seed
#' in each data source. 
#' @param datasources  a list of \code{\link[DSI]{DSConnection-class}} objects obtained after login. 
#' If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#' @return Sets the values of the vector of integers of length 626 known as
#' \code{.Random.seed} on each data source that is the true current state of the
#' random seed in each source. It also returns the value of the trigger
#' integer that has primed the random seed vector (\code{.Random.seed}) in
#' each source and also the integer vector of 626 elements
#' that is \code{.Random.seed itself}.
#' @author DataSHIELD Development Team
#' @examples 
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
#'   #Generate a pseudorandom number in the server-side
#'   
#'   ds.setSeed(seed.as.integer = 152584,
#'              datasources = connections)
#'              
#'   #Specify the pseudorandom number only in the first source
#'   
#'   ds.setSeed(seed.as.integer = 741,
#'              datasources = connections[1])#only the frist study is used (study1)
#'                    
#'   # Clear the Datashield R sessions and logout  
#'   datashield.logout(connections) 
#'   }
#' 
#' @export
ds.setSeed<-function(seed.as.integer=NULL,datasources=NULL){

##################################################################################
# look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }


seed.valid<-0

if(is.null(seed.as.integer)){
seed.as.text<-"NULL"
seed.valid<-1
}

if(is.numeric(seed.as.integer)){
seed.as.text<-as.character(seed.as.integer)
seed.valid<-1
}


if(seed.valid==0){
mess1<-("ERROR terminated: seed.as.integer must be set as an integer [numeric] or as being NULL")
return(mess1)
}

  calltext <- paste0("setSeedDS(", seed.as.text, ")")
  ssDS.obj <- DSI::datashield.aggregate(datasources, as.symbol(calltext))

  return.message<-paste0("Trigger integer to prime random seed = ",seed.as.text)

  return(list(status.message=return.message,seed.as.set=ssDS.obj))
}
# ds.setSeed
