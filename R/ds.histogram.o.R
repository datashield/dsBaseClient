#'
#' @title Generates a histogram plot
#' @description This function plots a non-disclosive histogram
#' @details It calls a datashield server side function that produces the
#' histogram objects to plot. Two options are possible as identified by the argument
#' \code{method}. The first option creates a histogram that excludes bins with
#' counts smaller than the allowed threshold. The second option creates a histogram
#' of the centroids of each k nearest neighbours. The function allows for the user to plot
#' disctinct histograms (one for each study) or a combine histogram that merges
#' the single plots.
#' @param x a charcater, the name of the vector of values for which the histogram is desired.
#' @param type a character which represent the type of graph to display. If \code{type} is set to
#' 'combine', a histogram that merges the single plot is displayed. Each histogram is plotted
#' separately if \code{type} is set to 'split'.
#' @param num.breaks a numeric specifying the number of breaks of the histogram. The default value
#' is set to 10.
#' @param method a character which defines which histogram will be created. If \code{method}
#' is set to 'smallCellsRule' (default option), the histogram of the actual variable is
#' created but bins with low counts are removed. If \code{method} is set to 'deterministic'
#' the histogram of the scaled centroids of each k nearest neighbours of the original variable
#' where the value of \code{k} is set by the user. If the \code{method} is set
#' to 'probabilistic', then the histogram shows the original distribution disturbed by the addition
#' of random stochastic noise. The added noise follows a normal distribution with zero mean and
#' variance equal to a percentage of the initial variance of the input variable. This percentage is
#' specified by the user in the argument \code{noise}.
#' @param k the number of the nearest neghbours for which their centroid is calculated.
#' The user can choose any value for k equal to or greater than the pre-specified threshold
#' used as a disclosure control for this method and lower than the number of observations
#' minus the value of this threshold. By default the value of k is set to be equal to 3
#' (we suggest k to be equal to, or bigger than, 3). Note that the function fails if the user
#' uses the default value but the study has set a bigger threshold. The value of k is used only
#' if the argument \code{method} is set to 'deterministic'. Any value of k is ignored if the
#' argument \code{method} is set to 'probabilistic' or 'smallCellsRule'.
#' @param noise the percentage of the initial variance that is used as the variance of the embedded
#' noise if the argument \code{method} is set to 'probabilistic'. Any value of noise is ignored if
#' the argument \code{method} is set to 'deterministic' or 'smallCellsRule'. The user can choose
#' any value for noise equal to or greater than the pre-specified threshold 'nfilter.noise'.
#' By default the value of noise is set to be equal to 0.25.
#' @param vertical.axis, a character which defines what is shown in the vertical axis of the
#' plot. If \code{vertical.axis} is set to 'Frequency' then the histogram of the frequencies
#' is returned. If \code{vertical.axis} is set to 'Density' then the histogram of the densities
#' is returned.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @return one or more histogram objects and plots depending on the argument \code{type}
#' @author Amadou Gaye, Demetris Avraam for DataSHIELD Development Team
#' @export
#' @examples
#' \dontrun{
#'
#'   # load that contains the login details
#'   data(logindata)
#'
#'   # login to the servers
#'   opals <- opal::datashield.login(logins=logindata, assign=TRUE)
#'
#'   # Example 1: generate a histogram for each study separately (the default behaviour)
#'   ds.histogram.o(x='LD$PM_BMI_CONTINUOUS', type="split")
#'
#'   # Example 2: generate a combined histogram with the default small cells counts
#'                suppression rule
#'   ds.histogram.o(x='LD$PM_BMI_CONTINUOUS', method='smallCellsRule', type='combine')
#'
#'   # Example 3: if a variable is of type factor then the function returns an error
#'   ds.histogram.o(x='LD$PM_BMI_CATEGORICAL')
#'
#'   # Example 4: generate a combined histogram with the deterministic method
#'   ds.histogram.o(x='LD$PM_BMI_CONTINUOUS', method='deterministic', type='combine')
#'
#'   # Example 5: same as Example 4 but with k=50
#'   ds.histogram.o(x='LD$PM_BMI_CONTINUOUS', k=50, method='deterministic', type='combine')
#'
#'   # Example 6: same as Example 4 but with k=1740 (here we see that as k increases we have
#'                big utility loss)
#'   ds.histogram.o(x='LD$PM_BMI_CONTINUOUS', k=1740, method='deterministic', type='combine')
#'
#'   # Example 7: same as Example 6 but for split analysis
#'   ds.histogram.o(x='LD$PM_BMI_CONTINUOUS', k=1740, method='deterministic', type='split')
#'
#'   # Example 7: if k is less than the pre-specified threshold then the function returns an error
#'   ds.histogram.o(x='LD$PM_BMI_CONTINUOUS', k=2, method='deterministic')
#'
#'   # Example 8: generate a combined histogram with the probabilistic method
#'   ds.histogram.o(x='LD$PM_BMI_CONTINUOUS', method='probabilistic', type='combine')
#'
#'   # Example 9: generate a histogram with the probabilistic method for each study separately
#'   ds.histogram.o(x='LD$PM_BMI_CONTINUOUS', method='probabilistic', type='split')
#'
#'   # Example 10: same as Example 9 but with higher level of noise
#'   ds.histogram.o(x='LD$PM_BMI_CONTINUOUS', method='probabilistic', noise=0.5, type='split')
#'
#'   # Example 11: if 'noise' is less than the pre-specified threshold then the function returns
#'                 an error
#'   ds.histogram.o(x='LD$PM_BMI_CONTINUOUS', method='probabilistic', noise=0.1, type='split')
#'
#'   # Example 12: same as Example 9 but with bigger number of breaks
#'   ds.histogram.o(x='LD$PM_BMI_CONTINUOUS', method='probabilistic', type='split', num.breaks=30)
#'
#'   # Example 13: same as Example 12 but the vertical axis shows densities instead of frequencies
#'   ds.histogram.o(x='LD$PM_BMI_CONTINUOUS', method='probabilistic', type='split', num.breaks=30,
#'                  vertical.axis='Density')
#'
#'   # Example 14: create a histogram and the probability density on the plot
#'   hist <- ds.histogram.o(x='LD$PM_BMI_CONTINUOUS', method='probabilistic', type='combine',
#'                          num.breaks=30, vertical.axis='Density')
#'   lines(hist$mids, hist$density)
#'
#'   # clear the Datashield R sessions and logout
#'   opal::datashield.logout(opals)
#'
#' }
#'
ds.histogram.o <- function(x=NULL, type="split", num.breaks=10, method="smallCellsRule", k=3, noise=0.25, vertical.axis="Frequency", datasources=NULL){

  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }

  if(is.null(x)){
    stop("Please provide the name of the input vector!", call.=FALSE)
  }

  # the input variable might be given as column table (i.e. D$x)
  # or just as a vector not attached to a table (i.e. x)
  # we have to make sure the function deals with each case
  xnames <- extract(x)
  varname <- xnames$elements
  obj2lookfor <- xnames$holders

  # check if the input object(s) is(are) defined in all the studies
  if(is.na(obj2lookfor)){
    defined <- isDefined(datasources, varname)
  }else{
    defined <- isDefined(datasources, obj2lookfor)
  }

  # call the internal function that checks the input object is of the same class in all studies.
  typ <- checkClass(datasources, x)

  # the input object must be a numeric or an integer vector
  if(typ != 'integer' & typ != 'numeric'){
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

  # call the server-side function that generates the histogram object to plot
  call <- paste0("histogramDS.o(", x, ",", num.breaks, ",", method.indicator, ",", k, ",", noise, ")")
  outputs <- opal::datashield.aggregate(datasources, call)

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
# ds.histogram.o
