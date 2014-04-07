#' 
#' @title Plots a histogram 
#' @description This function plots histogram of the given data values.
#' It calls a datashield server side function that produces the
#' histogram objects to plot. The objects to plot do not contain bins with
#' counts < 5. The function allows for the user to plot disctinct histograms
#' (one for each study) or a combine histogram that merges the single plots.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources. 
#' @param xvect vector of values for which the histogram is desired.
#' @param type a character which represent the type of graph to display. 
#' If \code{type} is set to 'combine', a histogram that merges the single 
#' plot is displayed. Each histogram is plotted separately if If \code{type} 
#' is set to 'split'.
#' @return one or more histogram plot depending on the argument \code{type}
#' @author Gaye, A.
#' @export
#' @examples {
#' 
#' # load that contains the login details
#' data(logindata)
#' 
#' # login and assign specific variable(s)
#' myvar <- list("LAB_TSC", "LAB_HDL")
#' opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#' # Example 1: plot a combined histogram of the variable 'LAB_TSC' - default behaviour
#' ds.histogram(datasources=opals, xvect=quote(D$LAB_TSC))
#' 
#' # Example 2: Plot the histograms of LAB_TSC separately (one per study)
#' ds.histogram(datasources=opals, xvect=quote(D$LAB_TSC), type="split")
#'
#' # Example 2: plot a combined histogram of the variable 'LAB_HDL' - default behaviour
#' ds.histogram(datasources=opals, xvect=quote(D$LAB_HDL))
#' 
#' # Example 3: plot the histograms of LAB_HDL separately (one per study)
#' ds.histogram(datasources=opals, xvect=quote(D$LAB_HDL), type="split")
#' 
#' # Example 4: Plot the histograms of the first and second study
#' ds.histogram(datasources=opals[1:2], xvect=quote(D$LAB_TSC), type="split")
#'
#' # Example 5: Plot the histogram of the third study only
#' ds.histogram(datasources=opals[3], xvect=quote(D$LAB_TSC), type="split")
#' }
#'
ds.histogram <- function(datasources=NULL, xvect=NULL, type="combine"){

  if(is.null(datasources)){
    message(" ALERT!")
    message(" No valid opal object(s) provided.")
    message(" Make sure you are logged in to valid opal server(s).")
    stop(" End of process!", call.=FALSE)
  }
  
  if(is.null(xvect)){
    message(" ALERT!")
    message(" Please provide a valid numeric vector")
    stop(" End of process!", call.=FALSE)
  }
  
  # get the name of the variable used for the histogram
  # the input variable might be given as column table (i.e. D$xvect)
  # or just as a vector not attached to a table (i.e. xvect)
  # we have to make sure the function deals with each case
  inputterms <- unlist(strsplit(deparse(xvect), "\\$", perl=TRUE))
  if(length(inputterms) > 1){
    variable <- strsplit(deparse(xvect), "\\$", perl=TRUE)[[1]][2]
  }else{
    variable <- deparse(xvect)
  }
  
  # study names
  stdnames <- names(datasources)
  
  # call the function that checks the variables are available and not empty
  vars2check <- list(xvect)
  datasources <- ds.checkvar(datasources, vars2check)
  
  # get the range from each studyand produce the 'global' range
  cally1 <- call("rangeDS", xvect) 
  ranges <- datashield.aggregate(datasources, cally1)
  minrs <- c()
  maxrs <- c()
  for(i in 1:length(ranges)){
    minrs <- append(minrs, ranges[[i]][1])
    maxrs <- append(maxrs, ranges[[i]][2])
  }
  range.arg <- c(min(minrs), max(maxrs))
  
  # call the function that produces the histogram object to plot
  # get the seed 
  seedval <- round(runif(1, 0, 1000))
  cally2 <- call("histogram.ds", xvect, range.arg[1], range.arg[2], seedval) 
  hist.objs <- vector("list", length(datasources))
  invalidcells <-  vector("list", length(datasources))
  outputs <- datashield.aggregate(datasources, cally2)
  for(i in 1: length(datasources)){
    output <- outputs[[i]]
    if(is.null(output)){
      stop(" Could not find equidistant break points that span all the data points, in stdnames[i]!")
    }
    hist.objs[[i]] <- output$histobject
    invalidcells[[i]] <- output$invalidcells
  }
  
  # combine the histogram objects 
  # 'breaks' and 'mids' are the same for all studies
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
  
  # plot the individual histograms on the same graph 
  # if the argument 'type'="combine" plot a combined histogram and if 'type'="split" plot single histograms separately
  if(type=="combine"){
    par(mfrow=c(1,1))
    plot(combined.histobject, xlab=variable, main='Histogram of the pooled data')
    return(combined.histobject)
  }else{  
    if(type=="split"){
      # set the graph area and plot
      ll <- length(datasources)
      if(ll > 1){
        if((ll %% 2) == 0){ numr <- ll/2 }else{ numr <- (ll+1)/2}
        numc <- 2
        par(mfrow=c(numr,numc))
        for(i in 1:ll){
          warning(names(datasources)[i], ": ", invalidcells[[i]], " invalid categories", immediate.=TRUE, call.=FALSE)
          plot(hist.objs[[i]], xlab=variable, main=paste("Histogram of ", names(datasources)[i], sep=""))
        }
        return(hist.objs)
      }else{
        par(mfrow=c(1,1))
        warning(names(datasources)[1], ": ", invalidcells[[1]], " invalid categories", immediate.=TRUE, call.=FALSE)
        plot(hist.objs[[1]], xlab=variable, main=paste("Histogram of ", names(datasources)[1], sep=""))
        return(hist.objs[[1]])
      }
    }else{
      stop('Function argument "type" has to be either "combine" or "split"')
    }
  }
  
  
}

