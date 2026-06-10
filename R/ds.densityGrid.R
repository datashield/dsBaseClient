#'
#' @title Generates a density grid in the client-side 
#' @description This function generates a grid density object which can then be used to produced
#' heatmap or contour plots.
#' @details The cells with a count > 0 and < nfilter.tab are considered invalid
#' and the count is set to 0.
#' 
#' In DataSHIELD the user does not have access to the micro-data so and extreme values
#' such as the maximum and the minimum are potentially non-disclosive so this function does not allow
#' for the user to set the limits of the density grid and 
#' the minimum and maximum values of the \code{x}
#' and \code{y} vectors. These elements are set by the server-side function 
#' \code{densityGridDS} to 'valid' values 
#' (i.e. values that do not lead to leakage of micro-data to the user).
#' 
#' Server function called: \code{densityGridDS}
#' @param x a character string providing the name of the input numerical  vector.
#' @param y a character string providing the name of the input numerical  vector.
#' @param numints an integer, the number of intervals for the grid density object. 
#' The default value is 20.
#' @param type a character string that represents the type of graph to display. 
#' If \code{type} is set to
#' \code{'combine'}, a pooled grid density matrix is generated, 
#' instead if \code{type} is set to \code{'split'}
#' one grid density matrix is generated. Default \code{'combine'}. 
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} objects obtained after login. 
#' If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#' @return \code{ds.densityGrid} returns a grid density matrix.  
#' @author DataSHIELD Development Team
#' @export
#' @examples
#' \dontrun{
#' 
#'  ## Version 6, for version 5 see the Wiki
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
#'   #Generate the density grid
#'   # Example1: generate a combined grid density object (default)
#'   ds.densityGrid(x="D$LAB_TSC",
#'                  y="D$LAB_HDL",
#'                  datasources = connections)#all opal servers are used
#'
#'   # Example2: generate a grid density object for each study separately
#'   ds.densityGrid(x="D$LAB_TSC",
#'                  y="D$LAB_HDL",
#'                  type="split",
#'                  datasources = connections[1])#only the first Opal server is used ("study1")
#'
#'   # Example3: generate a grid density object where the number of intervals is set to 15, for
#'   #           each study separately
#'   ds.densityGrid(x="D$LAB_TSC",
#'                  y="D$LAB_HDL",
#'                  type="split",
#'                  numints=15,
#'                  datasources = connections)
#'
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(connections)
#'
#' }
#'
ds.densityGrid <- function(x=NULL, y=NULL, numints=20, type='combine', datasources=NULL){

  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }

  if(is.null(x)){
    stop("Please provide the name of the numeric vector 'x'!", call.=FALSE)
  }

  if(is.null(y)){
    stop("Please provide the name of the numeric vector 'y'!", call.=FALSE)
  }

  # check if the input object is defined in all the studies
  isDefined(datasources, x)
  isDefined(datasources, y)

  # call the internal function that checks the input objects are of the same class in all studies.
  typ <- checkClass(datasources, x)
  typ <- checkClass(datasources, y)
  
  # name of the studies to be used in the plots' titles
  stdnames <- names(datasources)

  # number of studies
  num.sources <- length(datasources)

  if(type=="combine"){
    # get the range from each study and produce the 'global' range
    cally <- paste0('rangeDS(', x, ')')
    x.ranges <- DSI::datashield.aggregate(datasources, as.symbol(cally))

    cally <- paste0('rangeDS(', y, ')')
    y.ranges <- DSI::datashield.aggregate(datasources, as.symbol(cally))

    x.minrs <- c()
    x.maxrs <- c()
    y.minrs <- c()
    y.maxrs <- c()
    for(i in 1:num.sources){
      x.minrs <- append(x.minrs, x.ranges[[i]][1])
      x.maxrs <- append(x.maxrs, x.ranges[[i]][2])
      y.minrs <- append(y.minrs, y.ranges[[i]][1])
      y.maxrs <- append(y.maxrs, y.ranges[[i]][2])
    }
    x.range.arg <- c(min(x.minrs), max(x.maxrs))
    y.range.arg <- c(min(y.minrs), max(y.maxrs))

    x.global.min <- x.range.arg[1]
    x.global.max <- x.range.arg[2]
    y.global.min <- y.range.arg[1]
    y.global.max <- y.range.arg[2]

    # generate the grid density object to plot
    cally <- paste0("densityGridDS(", x, ",", y, ",", limits=T, ",", x.global.min, ",",
                    x.global.max, ",", y.global.min, ",", y.global.max, ",", numints, ")")
    grid.density.obj <- DSI::datashield.aggregate(datasources, as.symbol(cally))
    numcol <- dim(grid.density.obj[[1]])[2]

    # print the number of invalid cells in each participating study
    for (i in 1:num.sources){
      message(stdnames[i],': ', names(dimnames(grid.density.obj[[i]])[2]))
    }

    Global.grid.density <- matrix(0, dim(grid.density.obj[[1]])[1], numcol-2)
    for (i in 1:num.sources){
      Global.grid.density <- Global.grid.density + grid.density.obj[[i]][,1:(numcol-2)]
      names(dimnames(Global.grid.density))[2] <- "Grid Density Matrix of the Pooled Data"
    }
    # newline for some space between the previous messages and the matrix when it is displayed
    message()
    return(Global.grid.density)
  }else{
    if(type=="split"){
      # generate the grid density object
      num_intervals <- numints
      cally <- paste0("densityGridDS(", x, ",", y, ",", 'limits=FALSE', ",", 'x.min=NULL', ",",
                      'x.max=NULL', ",", 'y.min=NULL', ",", 'y.max=NULL', ",", numints=num_intervals, ")")
      grid.density.obj <- DSI::datashield.aggregate(datasources, as.symbol(cally))
      numcol <- dim(grid.density.obj[[1]])[2]

      # print the number of invalid cells in each participating study
      for (i in 1:num.sources){
        message(stdnames[i],': ', names(dimnames(grid.density.obj[[i]])[2]))
      }
      return(grid.density.obj)
    }else{
      stop('Function argument "type" has to be either "combine" or "split"')
    }
  }

}
