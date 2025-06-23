#'
#' @title Generates the summary of a server-side object
#' @description Generates the summary of a server-side object. 
#' @details This function provides some insight about an object. Unlike the similar native R 
#' \code{summary} function
#' only a limited class of objects can be used as input to reduce the risk of disclosure.
#' For example, the minimum and the maximum values of a numeric vector
#'  are not given to the client because they are potentially disclosive. 
#'  
#' server functions called: \code{isValidDS}, \code{dimDS} and \code{colnamesDS}
#' @param x a character string specifying the name of a numeric or factor variable.
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} objects obtained after login. 
#' If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#' @return \code{ds.summary} returns to the client-side the class and 
#' size of the server-side object. 
#' Also other information is returned depending on the class of the object.
#' For example, potentially disclosive information
#' such as the minimum and maximum values of numeric vectors are not returned. 
#' The summary is given for each study separately.
#' @author DataSHIELD Development Team
#' @export
#' @examples
#' \dontrun{
#'
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
#'   #Calculate the summary of a numeric variable
#'   
#'   ds.summary(x = "D$LAB_TSC",
#'              datasources = connections)
#'  
#'   #Calculate the summary of a factor variable
#' 
#'   ds.summary(x = "D$PM_BMI_CATEGORICAL",
#'              datasources = connections)
#'                                 
#'   # Clear the Datashield R sessions and logout  
#'   datashield.logout(connections) 
#'
#' }
#'
ds.summary <- function(x=NULL, datasources=NULL){

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

  # check if the input object is defined in all the studies
  isDefined(datasources, x)

  # call the internal function that checks if the input object is of the same class in all studies.
  typ <- checkClass(datasources, x)

  # the input object must be a numeric or an integer vector
  # the input object must be a dataframe or a factor
  if(!('data.frame' %in% typ) & !('character' %in% typ) & !('factor' %in% typ) & !('integer' %in% typ) & !('list' %in% typ) & !('logical' %in% typ) & !('matrix' %in% typ) & !('numeric' %in% typ)){
    stop("The input object must be a 'data.frame', 'character', factor', 'integer', 'list', 'logical', 'matrix' or 'numeric'.", call.=FALSE)
  }

  stdnames <- names(datasources)
  numsources <- length(datasources)
  finalOutput <- list()

  # now get the summary depending on the type of the input variable
  if(("data.frame" %in% typ) | ("matrix" %in% typ)){
    for(i in 1:numsources){
      validity <- DSI::datashield.aggregate(datasources[i], as.symbol(paste0('isValidDS(', x, ')')))[[1]]
      if(validity){
        dims <- DSI::datashield.aggregate(datasources[i], call('dimDS', x))
        r <- dims[[1]][1]
        c <- dims[[1]][2]
        cols <- (DSI::datashield.aggregate(datasources[i], call('colnamesDS', x)))[[1]]
        stdsummary <- list('class'=typ, 'number of rows'=r, 'number of columns'=c, 'variables held'=cols)
        finalOutput[[i]] <- stdsummary
      }else{
        finalOutput[[i]] <- 'INVALID object!'
      }
    }
    names(finalOutput) <- stdnames
  }

  if("character" %in% typ){
    for(i in 1:numsources){
      validity <- DSI::datashield.aggregate(datasources[i], as.symbol(paste0('isValidDS(', x, ')')))[[1]]
      if(validity){
        l <- DSI::datashield.aggregate(datasources[i], call('lengthDS', x))[[1]]
        stdsummary <- list('class'=typ, 'length'=l)
        finalOutput[[i]] <- stdsummary
      }else{
        finalOutput[[i]] <- 'INVALID object!'
      }
    }
    names(finalOutput) <- stdnames
  }

  if("factor" %in% typ){
    for(i in 1:numsources){
      validity <- DSI::datashield.aggregate(datasources[i], as.symbol(paste0('isValidDS(', x, ')')))[[1]]
      if(validity){
        l <- DSI::datashield.aggregate(datasources[i], call('lengthDS', x))[[1]]
        levels.resp <- DSI::datashield.aggregate(datasources[i], as.symbol(paste0('levelsDS(', x, ')' )))[[1]]
        categories <- levels.resp$Levels
        freq <- DSI::datashield.aggregate(datasources[i], as.symbol(paste0('table1DDS(', x, ')' )))[[1]][1]
        stdsummary <- list('class'=typ, 'length'=l, 'categories'=categories)
        for(j in 1:length(categories)){
          stdsummary[[3+j]] <- freq[[1]][1,j]
        }
        names(stdsummary)[4:(3+length(categories))] <- paste0("count of '", categories, "'")
        finalOutput[[i]] <- stdsummary
      }else{
        finalOutput[[i]] <- 'INVALID object!'
      }
    }
    names(finalOutput) <- stdnames
  }

  if(("integer" %in% typ) | ("numeric" %in% typ)){
    for(i in 1:numsources){
      validity <- DSI::datashield.aggregate(datasources[i], as.symbol(paste0('isValidDS(', x, ')')))[[1]]
      if(validity){
        l <- DSI::datashield.aggregate(datasources[i], call('lengthDS', x))[[1]]
        q <- (DSI::datashield.aggregate(datasources[i], as.symbol(paste0('quantileMeanDS(', x, ')' ))))[[1]]
        stdsummary <- list('class'=typ, 'length'=l, 'quantiles & mean'=q)
        finalOutput[[i]] <- stdsummary
      }else{
        finalOutput[[i]] <- 'INVALID object!'
      }

    }
    names(finalOutput) <- stdnames
  }

  if("list" %in% typ){
    for(i in 1:numsources){
      l <- DSI::datashield.aggregate(datasources[i], call('lengthDS', x))[[1]]
      elts <- DSI::datashield.aggregate(datasources[i], call('namesDS', x))
      if(length(elts) == 0){
        elts <- NULL
      }else{
        elts <- elts[[1]]
      }
      if(is.null(elts)){
        stdsummary <- list('class'=typ, 'length'=l)
      }else{
        stdsummary <- list('class'=typ, 'length'=l, 'elements held in the list'=elts)
      }
      finalOutput[[i]] <- stdsummary
    }
    names(finalOutput) <- stdnames
  }

  if("logical" %in% typ){
    for(i in 1:numsources){
      validity <- DSI::datashield.aggregate(datasources[i], as.symbol(paste0('isValidDS(', x, ')')))[[1]]
      if(validity){
        l <- DSI::datashield.aggregate(datasources[i], call('lengthDS', x))[[1]]
        freq <- DSI::datashield.aggregate(datasources[i], as.symbol(paste0('table1DDS(', x, ')' )))[[1]][1]
        stdsummary <- list('class'=typ, 'length'=l)
        for(j in 1:length(2)){
          stdsummary[[2+j]] <- freq[[1]][1,j]
        }
        names(stdsummary)[3:(2+2)] <- paste0("count of '", c('FALSE','TRUE'), "'")
        finalOutput[[i]] <- stdsummary
      }else{
        finalOutput[[i]] <- 'INVALID object!'
      }
    }
    names(finalOutput) <- stdnames
  }

  return(finalOutput)
}
