#'
#' @title Generates a histogram plot
#' @description \code{ds.histogram} function plots a non-disclosive histogram in the client-side. 
#' @details \code{ds.histogram} function allows the user to plot
#' distinct histograms (one for each study) or a combined histogram that merges
#' the single plots.
#' 
#' In the argument \code{type} can be specified two types of graphics to display:
#'  \describe{
#'    \item{\code{'combine'}}{: a histogram that merges the single plot is displayed.} 
#'    \item{\code{'split'}}{: each histogram is plotted separately.}
#'     }
#'     
#' In the argument \code{method} can be specified 3 different histograms to be created:
#'  \describe{
#'    \item{\code{'smallCellsRule'}}{: the histogram of the actual variable is
#'           created but bins with low counts are removed.} 
#'    \item{\code{'deterministic'}}{: the histogram of the scaled centroids of each 
#'          \code{k} nearest neighbours of the original variable
#'          where the value of \code{k} is set by the user.} 
#'    \item{\code{'probabilistic'}}{: the histogram shows the original distribution disturbed 
#'          by the addition of random stochastic noise.
#'          The added noise follows a normal distribution with zero mean and
#'          variance equal to a percentage of the initial variance of the input variable. 
#'          This percentage is specified by the user in the argument \code{noise}.} 
#'  
#'     }
#' 
#' 
#' In the \code{k} argument the user can choose any value for \code{k} equal 
#' to or greater than the pre-specified threshold
#' used as a disclosure control for this method and lower than the number of observations
#' minus the value of this threshold. By default the value of \code{k} is set to be equal to 3
#' (we suggest k to be equal to, or bigger than, 3). Note that the function fails if the user
#' uses the default value but the study has set a bigger threshold. 
#' The value of \code{k} is used only if the argument 
#' \code{method} is set to \code{'deterministic'}. 
#' Any value of k is ignored if the
#' argument \code{method} is set to \code{'probabilistic'} or \code{'smallCellsRule'}.
#' 
#' In the \code{noise} argument the percentage of the initial variance 
#' that is used as the variance of the embedded
#' noise if the argument \code{method} is set to \code{'probabilistic'}. 
#' Any value of noise is ignored if the argument 
#' \code{method} is set to \code{'deterministic'} or \code{'smallCellsRule'}. 
#' The user can choose any value for noise equal to or greater 
#' than the pre-specified threshold \code{'nfilter.noise'}.
#' By default the value of noise is set to be equal to 0.25.
#' 
#' In the argument  \code{vertical.axis} can be specified two types of histograms:
#' \describe{
#'    \item{\code{'Frequency'}}{: the histogram of the frequencies
#'     is returned.} 
#'    \item{\code{'Density'}}{: the histogram of the densities
#'     is returned.}
#'     }
#' 
#' Server function called: \code{histogramDS2}
#' @param x a character string specifying the name of a numerical vector.
#' @param type a character string that represents the type of graph to display.
#' The \code{type} argument can be set as \code{'combine'} or \code{'split'}. 
#' Default \code{'split'}.
#' For more information see \strong{Details}.
#' @param num.breaks a numeric specifying the number of breaks of the histogram. Default value
#' is \code{10}.
#' @param method a character string that defines which histogram will be created.
#' The \code{method} argument can be set as \code{'smallCellsRule'}, 
#' \code{'deterministic'} or \code{'probabilistic'}. 
#' Default \code{'smallCellsRule'}.  
#' For more information see \strong{Details}.
#' @param k the number of the nearest neighbours for which their centroid is calculated. 
#' Default \code{k} value is \code{3}.
#' For more information see \strong{Details}. 
#' @param noise the percentage of the initial variance that is used as the variance of the embedded
#' noise if the argument \code{method} is set to \code{'probabilistic'}. 
#' Default \code{noise} value is  \code{0.25}.
#' For more information see \strong{Details}.
#' @param vertical.axis, a character string that defines what is shown in the vertical axis of the
#' plot. The \code{vertical.axis} argument can be set as \code{'Frequency'} or \code{'Density'}.
#' Default \code{'Frequency'}. 
#' For more information see \strong{Details}.
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} objects obtained after login. 
#' If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#' @return one or more histogram objects and plots depending on the argument \code{type}
#' @author DataSHIELD Development Team
#' @export
#' @examples
#' \dontrun{
#'
#' ## Version 6, for version 5 see the Wiki
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
#'   # Compute the histogram
#'   # Example 1: generate a histogram for each study separately 
#'   ds.histogram(x = 'D$PM_BMI_CONTINUOUS',
#'               type = "split",
#'               datasources = connections) #all studies are used
#'
#'   # Example 2: generate a combined histogram with the default small cells counts
#'                suppression rule
#'   ds.histogram(x = 'D$PM_BMI_CONTINUOUS',
#'                method = 'smallCellsRule',
#'                type = 'combine',
#'                datasources = connections[1]) #only the first study is used (study1)
#'
#'   # Example 3: if a variable is of type factor the function returns an error
#'   ds.histogram(x = 'D$PM_BMI_CATEGORICAL',
#'                datasources = connections)
#'
#'   # Example 4: generate a combined histogram with the deterministic method for k=50
#'   ds.histogram(x = 'D$PM_BMI_CONTINUOUS',
#'                k = 50, 
#'                method = 'deterministic',
#'                type = 'combine',
#'                datasources = connections[2])#only the second study is used (study2)
#'
#'
#'   # Example 5: create a histogram and the probability density on the plot
#'   hist <- ds.histogram(x = 'D$PM_BMI_CONTINUOUS',
#'                        method = 'probabilistic', type='combine',
#'                        num.breaks = 30, 
#'                        vertical.axis = 'Density',
#'                        datasources = connections)
#'   lines(hist$mids, hist$density)
#' 
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(connections)
#'   }
#'
#'
ds.histogram <- function(x=NULL, type="split", num.breaks=10, method="smallCellsRule", k=3, noise=0.25, vertical.axis="Frequency", datasources=NULL){

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

  # call the internal function that checks the input object is of the same class in all studies.
  typ <- checkClass(datasources, x)

  # the input object must be a numeric or an integer vector
  if(!('integer' %in% typ) & !('numeric' %in% typ)){
    message(paste0(x, " is of type ", typ, "!"))
    stop("The input object must be an integer or numeric vector.", call.=FALSE)
  }

  # the argument vertical.axis must be "Frequency" or "Density"
  if(vertical.axis != 'Frequency' & vertical.axis != 'Density'){
    stop('Function argument "vertical.axis" has to be either "Frequency" or "Density"', call.=FALSE)
  }

  # the argument method must be either "smallCellsRule" or "deterministic" or "probabilistic"
  if(method != 'smallCellsRule' & method != 'deterministic' & method != 'probabilistic'){
    stop('Function argument "method" has to be either "smallCellsRule" or "deterministic" or "probabilistic"', call.=FALSE)
  }

  # name of the studies to be used in the plots' titles
  stdnames <- names(datasources)

  # number of studies
  num.sources <- length(datasources)

  if(method=='smallCellsRule'){ method.indicator <- 1 }
  if(method=='deterministic'){ method.indicator <- 2 }
  if(method=='probabilistic'){ method.indicator <- 3 }

  # call the server-side function that returns the range of the vector from each study
  cally1 <- paste0("histogramDS1(", x, ",", method.indicator, ",", k, ",", noise, ")")
  ranges <- unique(unlist(DSI::datashield.aggregate(datasources, as.symbol(cally1))))

  # produce the 'global' range
  range.arg <- c(min(ranges, na.rm=TRUE), max(ranges, na.rm=TRUE))
  min <- range.arg[1]
  max <- range.arg[2]
  
  # get the axis label
  xnames <- extract(x)
  varname <- xnames$elements

  # call the server-side function that generates the histogram object to plot
  call <- paste0("histogramDS2(", x, ",", num.breaks, ",", min, ",", max, ",", method.indicator, ",", k, ",", noise, ")")
  outputs <- DSI::datashield.aggregate(datasources, call)

  hist.objs <- vector("list", length(datasources))
  invalidcells <- vector("list", length(datasources))

  for(i in 1:length(datasources)){
    output <- outputs[[i]]
    if(is.null(output)){
      stop("Equidistant break points that span all the data points could not be find, in stdnames[i]!")
    }
    hist.objs[[i]] <- output$histobject
    invalidcells[[i]] <- output$invalidcells
  }

  # if type is set to 'combine' then combine the histogram objects
  # 'breaks' and 'mids' are the same for all studies
  if(type=='combine'){
    global.counts <- rep(0, length(hist.objs[[1]]$counts))
    global.density <- rep(0, length(hist.objs[[1]]$density))
    for(i in 1:length(datasources)){
      global.counts <- global.counts + hist.objs[[i]]$counts
      global.density <- global.density + hist.objs[[i]]$density
    }
    global.density <- global.density/3
    global.intensities <- global.density

    # generate the combined histogram object to plot
    combined.histobject <- hist.objs[[1]]
    combined.histobject$counts <- global.counts
    combined.histobject$density <- global.density
    combined.histobject$intensities <- combined.histobject$density
  }

  # plot the individual histograms on the same graph
  # if the argument 'type'="combine" plot a combined histogram and if 'type'="split" plot single
  # histograms for each study separately
  if(type=="combine"){
    graphics::par(mfrow=c(1,1))
    if(vertical.axis=="Frequency"){
      graphics::plot(combined.histobject, freq=TRUE, xlab=varname, main='Histogram of the pooled data')
    }
    if(vertical.axis=="Density"){
      graphics::plot(combined.histobject, freq=FALSE, xlab=varname, main='Histogram of the pooled data')
    }
    return(combined.histobject)
  }else{
    if(type=="split"){
      # set the graph area and plot
      ll <- length(datasources)
      if(ll > 1){
        if((ll %% 2) == 0){ numr <- ll/2 }else{ numr <- (ll+1)/2}
        numc <- 2
        graphics::par(mfrow=c(numr,numc))
        for(i in 1:ll){
          warning(names(datasources)[i], ": ", invalidcells[[i]], " invalid cells", immediate.=TRUE, call.=FALSE)
          if(vertical.axis=="Frequency"){
            graphics::plot(hist.objs[[i]], freq=TRUE, xlab=varname, main=paste("Histogram of ", names(datasources)[i], sep=""))
          }
          if(vertical.axis=="Density"){
            graphics::plot(hist.objs[[i]], freq=FALSE, xlab=varname, main=paste("Histogram of ", names(datasources)[i], sep=""))
          }
        }
        return(hist.objs)
      }else{
        graphics::par(mfrow=c(1,1))
        warning(names(datasources)[1], ": ", invalidcells[[1]], " invalid cells", immediate.=TRUE, call.=FALSE)
        if(vertical.axis=="Frequency"){
          graphics::plot(hist.objs[[1]], freq=TRUE, xlab=varname, main=paste("Histogram of ", names(datasources)[1], sep=""))
        }
        if(vertical.axis=="Density"){
          graphics::plot(hist.objs[[1]], freq=FALSE, xlab=varname, main=paste("Histogram of ", names(datasources)[1], sep=""))
        }
        return(hist.objs[[1]])
      }
    }else{
      stop('Function argument "type" has to be either "combine" or "split"')
    }
  }

}
# ds.histogram
