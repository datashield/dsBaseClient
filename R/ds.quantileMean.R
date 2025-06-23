#'
#' @title Computes the quantiles of a server-side variable
#' @description This function calculates the mean and quantile values of a 
#' server-side quantitative variable. 
#' @details This function does not return the minimum and maximum values
#' because they are potentially disclosive.
#' 
#' Depending on the argument \code{type} can be carried out two types of analysis: \cr
#' (1) \code{type = 'combine'} pooled values are displayed \cr
#' (2) \code{type = 'split'} summaries are
#' returned for each study. 
#' 
#' Server functions called: \code{quantileMeanDS}, \code{length} and \code{numNaDS}
#' @param x a character string specifying the name of the numeric vector. 
#' @param type a character that represents the type of graph to display.
#' This can be set as \code{'combine'} or \code{'split'}.
#' For more information see \strong{Details}. 
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} 
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#' @return \code{ds.quantileMean} returns to the client-side the quantiles and statistical mean
#' of a server-side numeric vector. 
#' @author DataSHIELD Development Team
#' @seealso \code{\link{ds.mean}} to compute the statistical mean.
#' @seealso \code{\link{ds.summary}} to generate the summary of a variable.
#' @export
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
#'   #Get the quantiles and mean of a server-side variable
#'   
#'   ds.quantileMean(x = "D$LAB_TRIG",
#'                   type = "combine",
#'                   datasources = connections)
#'   
#'   
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(connections)
#'
#'
#' }
#'
ds.quantileMean <- function(x=NULL, type='combine', datasources=NULL){

  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }

  if(is.null(x)){
    stop("Please provide the name of the input vector!", call.=FALSE)
  }

  if (! all(type %in% c("combine", "split"))) {
    stop('Function argument "type" has to be either "combine" or "split"', call.=FALSE)
  }

  # check if the input object is defined in all the studies
  isDefined(datasources, x)

  # call the internal function that checks the input object is of the same class in all studies.
  typ <- checkClass(datasources, x)

  # the input object must be a numeric or an integer vector
  if(!('integer' %in% typ) & !('numeric' %in% typ)){
    message(paste0(x, " is of type ", typ, "!"))
    stop("The input object must be an integer or numeric vector.", call.=FALSE)
  }

  # get the server function that produces the quantiles
  cally1 <- paste0('quantileMeanDS(', x, ')')
  quants <- DSI::datashield.aggregate(datasources, as.symbol(cally1))

  # combine the vector of quantiles - using weighted sum
  cally2 <- call('lengthDS', x)
  lengths <- DSI::datashield.aggregate(datasources, cally2)
  cally3 <- paste0("numNaDS(", x, ")")
  numNAs <- DSI::datashield.aggregate(datasources, as.symbol(cally3))
  global.quantiles <- rep(0, length(quants[[1]])-1)
  global.mean <- 0
  for(i in 1: length(datasources)){
    vect <- quants[[i]][1:7] * (lengths[[i]]-numNAs[[i]])
    global.quantiles <- global.quantiles + vect
    global.mean <- global.mean + quants[[i]][8] * (lengths[[i]]-numNAs[[i]])
  }

  global.mean <- global.mean/(sum(unlist(lengths))-sum(unlist(numNAs)))
  global.quantiles <- global.quantiles/(sum(unlist(lengths))-sum(unlist(numNAs)))
  output <- c(global.quantiles, global.mean)
  names(output) <- c("5%","10%","25%","50%","75%","90%","95%","Mean")

  if(type=="combine"){
    message(" Quantiles of the pooled data")
    return(output)
  }else{
    if(type=="split"){
      return(quants)
    }else{
      stop('Function argument "type" has to be either "combine" or "split"', call.=FALSE)
    }
  }
}
