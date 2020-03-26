#'
#' @title Tests for correlation between paired samples in the server-side
#' @description This is similar to the R base function \code{cor.test}.
#' @details Runs a two-sided Pearson test with a 0.95 confidence level.
#' 
#' Server function called: \code{cor.test}
#' @param x a character string providing  the name of a numerical vector. 
#' @param y a character string providing  the name of a numerical vector. 
#' @param datasources a list of \code{\link{DSConnection-class}} 
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link{datashield.connections_default}}.
#' @return \code{ds.corTest} returns to the client-side the results of the Pearson test. 
#' @author DataSHIELD Development Team
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
#'   # test for correlation
#'   ds.corTest(x = "D$LAB_TSC",
#'              y = "D$LAB_HDL",
#'              datasources = connections[1]) #Only first server is used ("study1")
#'                 
#'   # Clear the Datashield R sessions and logout                 
#'   datashield.logout(connections) 
#'   
#' }   
#'
#'
ds.corTest = function(x=NULL, y=NULL, datasources=NULL){

  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  if(is.null(x)){
    stop("x=NULL. Please provide the names of the 1st vector!", call.=FALSE)
  }
  if(is.null(y)){
    stop("y=NULL. Please provide the names of the 2nd numeric vector!", call.=FALSE)
  }

  # the input variable might be given as column table (i.e. D$object)
  # or just as a vector not attached to a table (i.e. object)
  # we have to make sure the function deals with each case
  objects <- c(x,y)
  xnames <- extract(objects)
  varnames <- xnames$elements
  obj2lookfor <- xnames$holders

  # check if the input object(s) is(are) defined in all the studies
  for(i in 1:length(varnames)){
    if(is.na(obj2lookfor[i])){
      defined <- isDefined(datasources, varnames[i])
    }else{
      defined <- isDefined(datasources, obj2lookfor[i])
    }
  }

  # call the internal function that checks the input object(s) is(are) of the same class in all studies.
  for(i in 1:length(objects)){
    typ <- checkClass(datasources, objects[i])
  }

  # call the server side function
  cally <- paste0("cor.test(", x, ",", y, ")")
  res.local <- DSI::datashield.aggregate(datasources, as.symbol(cally))

  return(res.local)

}
