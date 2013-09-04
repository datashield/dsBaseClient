#' 
#' @title Plots a histogram 
#' @description This function plots histogram of the given data values.
#' It calls a datashield server side function that produces the
#' histogram objects to plot. The objects to plot do not contain bins with
#' counts < 5. The function allows for the user to plot disctinct histograms
#' (one for each study) or a combine histogram that merges the single plots.
#' @param opals a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal 
#' datasources. 
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
#' myvar <- list("LAB_TSC")
#' opals <- ds.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#' # Example 1: plot a combined histogram of the variable 'LAB_TSC' - default behaviour
#' ds.histogram(opals=opals, xvect=quote(D$LAB_TSC))
#' 
#' # Example 2: Plot the histograms separately (one per study)
#'  ds.histogram(opals=opals, xvect=quote(D$LAB_TSC), type="split")
#'  
#' # Example 3: Plot the histograms of the first and second study
#'  ds.histogram(opals=opals[1:2], xvect=quote(D$LAB_TSC), type="split")
#'
#' # Example 4: Plot the histogram of the third study only
#'  ds.histogram(opals=opals[3], xvect=quote(D$LAB_TSC), type="split")
#' }
#'
ds.histogram <- function(opals=opals, xvect=NULL, type="combine"){


  if(is.null(opals)){
    cat("\n\n ALERT!\n")
    cat(" No valid opal object(s) provided.\n")
    cat(" Make sure you are logged in to valid opal server(s).\n")
    stop(" End of process!\n\n", call.=FALSE)
  }
  
  if(is.null(xvect)){
    cat("\n\n ALERT!\n")
    cat(" Please provide a valid numeric vector\n")
    stop(" End of process!\n\n", call.=FALSE)
  }
  
  # get the name of the variable used for the histogram
  variable <-  strsplit(deparse(xvect), "\\$", perl=TRUE)[[1]][2]
  
  # call the function that checks the variables are available and not empty
  vars2check <- list(xvect)
  opals <- ds.checkvar(opals, vars2check)
  
  # get the range from each studyand produce the 'global' range
  cally1 <- call("range.ds", xvect) 
  ranges <- datashield.aggregate(opals, cally1)
  minrs <- c()
  maxrs <- c()
  for(i in 1:length(ranges)){
    minrs <- append(minrs, ranges[[i]][1])
    maxrs <- append(maxrs, ranges[[i]][2])
  }
  range.arg <- c(min(minrs), max(maxrs))
  
  # get the global break points and ensure that 
  # the breaks do span the range of xvect on all studies
  brks <- seq(range.arg[1], range.arg[2], 0.3)
  if(min(brks) > range.arg[1] || max(brks) < range.arg[2]){
    while(min(brks) > range.arg[1] || max(brks) < range.arg[2]){
      brks <- seq(range.arg[1]*runif(1,0.9,1), range.arg[2]*runif(1,1,1.1), 0.3)
    }
  }

  # call the function that produces the histogram object to plot
  cally2 <- call("histogram.ds", xvect, brks) 
  hist.objs <- vector("list", length(opals))
  asterix2plot <-  vector("list", length(opals))
  for(i in 1: length(opals)){
    output <- datashield.aggregate(opals[i], cally2)
    hist.objs[[i]] <- output[[1]]$histobject
    asterix2plot[[i]] <- output[[1]]$aterix2plot
  }
  
  # combine the histogram objects 
  # 'breaks' and 'mids' are the same for all studies
  global.counts <- rep(0, length(hist.objs[[i]]$counts))
  global.density <- rep(0, length(hist.objs[[i]]$density))
  global.intensities <- rep(0, length(hist.objs[[i]]$intensities))
  for(i in 1:length(opals)){
    global.counts <- global.counts + hist.objs[[i]]$counts
    global.density <- global.density + hist.objs[[i]]$density
    global.intensities <- global.intensities + hist.objs[[i]]$intensities    
  }
  global.density <- global.density/3
  global.intensities <- global.intensities/3
  
  # generate the combined histogram object to plot
  combined.histobject <- hist.objs[[1]]
  combined.histobject$counts <- global.counts
  combined.histobject$density <- global.density
  combined.histobject$intensities <- global.intensities
  
  # plot the individual histograms on the same graph 
  # if the argument 'type'="combine" plot a combined histogram and if 'type'="split" plot single histograms separately
  if(type=="combine"){
    par(mfrow=c(1,1))
    plot(combined.histobject, xlab=variable, main='Histogram of the pooled data')
  }else{  
    if(type=="split"){
      # set the graph area and plot
      ll <- length(opals)
      if(ll > 1){
        if((ll %% 2) == 0){ numr <- ll/2 }else{ numr <- (ll+1)/2}
        numc <- 2
        par(mfrow=c(numr,numc))
        for(i in 1:ll){
          plot(hist.objs[[i]], xlab=variable, main=paste("Histogram of ", names(opals)[i], sep=""))
          # if there are cells with count > 0 and < mention them as an '*' on the graph
          if(length(asterix2plot[[i]]) > 0){
            text(asterix2plot[[i]], rep(7.5, length(asterix2plot[[i]])), "*", pos=3, cex=1.2)
            legend('topleft', "(*)\ncell count\n> 0 &\n< 5", bty='n', cex=0.8)
          }
        }
      }else{
        par(mfrow=c(1,1))
        plot(hist.objs[[1]], xlab=variable, main=paste("Histogram of ", names(opals)[1], sep=""))
        # if there are cells with count > 0 and < mention them as an '*' on the graph
        if(length(asterix2plot[[1]]) > 0){
          text(asterix2plot[[1]], rep(7.5, length(asterix2plot[[1]])), "*", pos=3, cex=1.2)
          legend('topleft', "(*)\ncell count\n> 0 &\n< 5", bty='n', cex=0.8)
        }
      }
    }else{
      stop('Function argument "type" has to be either "combine" or "split"')
    }
  }
}

