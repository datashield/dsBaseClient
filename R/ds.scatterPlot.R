#'
#' @title Generates non-disclosive scatter plots
#' @description This function uses two disclosure control methods to generate non-disclosive
#' scatter plots of two server-side continuous variables. 
#' @details As the generation of a scatter plot from original data is disclosive and is not
#' permitted in DataSHIELD, this function allows the user to plot non-disclosive scatter plots.
#' 
#' If the argument \code{method} is set to \code{'deterministic'}, the server-side function searches
#' for the \code{k-1} nearest neighbours of each single data point and calculates the centroid 
#' of such \code{k} points. 
#' The proximity is defined by the minimum Euclidean distances of z-score transformed data.
#' 
#' When the coordinates of all centroids are estimated the function applies scaling to expand the
#' centroids back to the dispersion of the original data. The scaling is achieved by multiplying
#' the centroids with a scaling factor that is equal to the ratio between the standard deviation of
#' the original variable and the standard deviation of the calculated centroids. The coordinates of
#' the scaled centroids are then returned to the client-side.
#' 
#' The value of \code{k} is specified by the user. 
#' The suggested and default value is equal to 3 which is also
#' the suggested minimum threshold that is used to prevent disclosure which is specified in the
#' protection filter \code{nfilter.kNN}. When the value of \code{k} increases, 
#' the disclosure risk decreases but the utility loss increases.
#' The value of \code{k} is used only
#' if the argument \code{method} is set to \code{'deterministic'}. 
#' Any value of \code{k} is ignored if the
#' argument \code{method} is set to \code{'probabilistic'}.
#' 
#' If the argument \code{method} is set to \code{'probabilistic'}, 
#' the server-side function generates a random normal noise of zero mean
#' and variance equal to 10\% of the variance of each \code{x} and \code{y} variable.
#' The noise is added to each \code{x} and \code{y} variable and the disturbed by the addition of
#' \code{noise} data are returned to the client-side. Note that the seed random number generator is fixed to a
#' specific number generated from the data and therefore the user gets the same figure every time
#' that chooses the probabilistic method in a given set of variables.
#' The value of \code{noise} is used only if the argument \code{method} is set to \code{'probabilistic'}.
#' Any value of \code{noise} is ignored if
#' the argument \code{method} is set to \code{'deterministic'}. 
#' 
#' In \code{type} argument can be set two graphics to display:\cr
#' (1) If \code{type = 'combine'}  a scatter plot for
#' combined data is generated.\cr
#' (2) If \code{type = 'split'}  one scatter plot for each
#' study is generated. 
#' 
#' Server function called: \code{scatterPlotDS}
#' @param x a character string specifying the name of the explanatory variable, a numeric vector. 
#' @param y a character string specifying the name of the response variable,  a numeric vector.
#' @param method a character string that specifies the 
#' method that is used to generated non-disclosive
#' coordinates to be displayed in a scatter plot. 
#' This argument can be set as \code{'deteministic'} or \code{'probabilistic'}.
#' Default \code{'deteministic'}. 
#' For more information see \strong{Details}. 
#' @param k the number of the nearest neighbours  for which their centroid is calculated.
#' Default 3. 
#' For more information see \strong{Details}. 
#' @param noise the percentage of the initial variance that is used as the variance of the embedded
#' noise if the argument \code{method} is set to \code{'probabilistic'}.
#' For more information see \strong{Details}. 
#' @param type a character that represents the type of graph to display.
#' This can be set as \code{'combine'} or \code{'split'}. 
#' Default \code{'split'}. 
#' For more information see \strong{Details}.
#' @param return.coords a logical. If TRUE the coordinates of the anonymised data points are return 
#' to the Console. Default value is FALSE.
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} objects obtained after login. 
#' If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#' @return \code{ds.scatterPlot} returns to the client-side one or more scatter 
#' plots depending on the argument \code{type}. 
#' @author DataSHIELD Development Team
#' @export
#' @examples
#' \dontrun{
#'
#'   ## Version 6, for version 5 see the Wiki 
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

#'   # Log onto the remote Opal training servers
#'   connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D") 
#'
#'   #Example 1: generate a scatter plot for each study separately
#'   #Using the default deterministic method and k = 10
#'   
#'   ds.scatterPlot(x = "D$PM_BMI_CONTINUOUS",
#'                  y = "D$LAB_GLUC_ADJUSTED",
#'                  method = "deterministic",
#'                  k = 10,
#'                  type = "split",
#'                  datasources = connections)
#'
#'   #Example 2: generate a combined scatter plot with the probabilistic method
#'   #and noise of variance 0.5% of the variable's variance, and display the coordinates
#'   # of the anonymised data points to the Console
#'   
#'   ds.scatterPlot(x = "D$PM_BMI_CONTINUOUS",
#'                  y = "D$LAB_GLUC_ADJUSTED",
#'                  method = "probabilistic",
#'                  noise = 0.5,
#'                  type = "combine",
#'                  datasources = connections)
#'                    
#'   #Clear the Datashield R sessions and logout
#'   datashield.logout(connections) 
#'
#' }
#'
ds.scatterPlot <- function(x=NULL, y=NULL, method='deterministic', k=3, noise=0.25, type="split", return.coords=FALSE, datasources=NULL){

  if(is.null(x)){
    stop("Please provide the name of the x-variable", call.=FALSE)
  }

  if(is.null(y)){
    stop("Please provide the name of the y-variable", call.=FALSE)
  }

  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }

  # check if the input objects are defined in all the studies
  isDefined(datasources, x)
  isDefined(datasources, y)

  # call the internal function that checks the input object(s) is(are) of the same class in all studies.
  typ.x <- checkClass(datasources, x)
  typ.y <- checkClass(datasources, y)

  # the input objects must be numeric or integer vectors
  if(!('integer' %in% typ.x) & !('numeric' %in% typ.x)){
    message(paste0(x, " is of type ", typ.x, "!"))
    stop("The input objects must be integer or numeric vectors.", call.=FALSE)
  }
  if(!('integer' %in% typ.y) & !('numeric' %in% typ.y)){
    message(paste0(y, " is of type ", typ.y, "!"))
    stop("The input objects must be integer or numeric vectors.", call.=FALSE)
  }

  # get the axes labels
  xnames <- extract(x)
  x.lab <- xnames[[length(xnames)]]
  ynames <- extract(y)
  y.lab <- ynames[[length(ynames)]]

  # name of the studies to be used in the plots' titles
  stdnames <- names(datasources)

  # number of studies
  num.sources <- length(datasources)

  if(method=='deterministic'){ method.indicator <- 1 }
  if(method=='probabilistic'){ method.indicator <- 2 }

  # call the server-side function that generates the x and y coordinates of the centroids
  call <- paste0("scatterPlotDS(", x, ",", y, ",", method.indicator, ",", k, ",", noise, ")")
  output <- DSI::datashield.aggregate(datasources, call)

  pooled.points.x <- c()
  pooled.points.y <- c()
  for (i in 1:num.sources){
    pooled.points.x[[i]] <- output[[i]][[1]]
    pooled.points.y[[i]] <- output[[i]][[2]]
  }
  pooled.points.x <- unlist(pooled.points.x)
  pooled.points.y <- unlist(pooled.points.y)
  pooled.coordinates <- cbind(x=pooled.points.x, y=pooled.points.y)

  # plot and return the scatter plot depending on the argument "type"
  if(type=="combine"){
    numr <- 1
    numc <- 1
    graphics::par(mfrow=c(numr,numc))
    graphics::plot(pooled.points.x, pooled.points.y, xlab=x.lab, ylab=y.lab, main=paste0("Combined scatter plot"))
    return.message <- "Combined plot created"
    if(isTRUE(return.coords)){
      return(list(pooled.coordinates=pooled.coordinates, message=return.message))
    }else{
      return(message=return.message)
    }  
  }else{
    if(type=="split"){
      # set the graph area and plot
      if(num.sources > 1){
        if((num.sources %% 2) == 0){ numr <- num.sources/2 }else{ numr <- (num.sources+1)/2}
          numc <- 2
          graphics::par(mfrow=c(numr,numc))
          scatter <- list()
      }
        split.coordinates <- list()
        for(i in 1:num.sources){
          title <- paste0("Scatter plot of ", stdnames[i])
          x <- output[[i]][[1]]
          y <- output[[i]][[2]]
          graphics::plot(x, y, xlab=x.lab, ylab=y.lab, main=title)
          split.coordinates[[i]] <- cbind(x=output[[i]][[1]], y=output[[i]][[2]])
        }
        names(split.coordinates) <- stdnames
        return.message <- "Split plot created"
        if(isTRUE(return.coords)){
          return(list(split.coordinates, message=return.message))
        }else{
          return(message=return.message)
        }  
    }else{
      stop('Function argument "type" has to be either "combine" or "split"')
    }
  }
}
#ds.scatterPlot
