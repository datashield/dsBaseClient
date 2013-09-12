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
#' myvar <- list("LAB_HDL")
#' opals <- ds.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#' # Example 1: plot a combined histogram of the variable 'LAB_HDL' - default behaviour
#' ds.quantilemean(opals=opals, xvect=quote(D$LAB_HDL))
#' 
#' # Example 2: Plot the histograms separately (one per study)
#' ds.quantilemean(opals=opals, xvect=quote(D$LAB_HDL), type="split")
#' 
#' }
#'
ds.quantilemean <- function(opals=NULL, xvect=NULL, type="combine"){
  
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
  
  # names of the opals/studies
  stdnames <- names(opals)
  
  # get the name of the input variable
  variable <-  strsplit(deparse(xvect), "\\$", perl=TRUE)[[1]][2]
  
  # call the function that checks the variable is available and not empty
  vars2check <- list(xvect)
  opals <- ds.checkvar(opals, vars2check)
  
  # get the server function that produces the quantiles
  cally1 <- call("quantilemean.ds", xvect) 
  quants <- datashield.aggregate(opals, cally1)
  
  # combine the vector of quantiles - using weighted sum
  cally2 <- call("length", xvect) 
  lengths <- datashield.aggregate(opals, cally2)
  global.quantiles <- rep(0, length(quants[[1]])-1)
  global.mean <- 0
  for(i in 1: length(opals)){
    vect <- quants[[i]][1:7] * lengths[[i]]
    global.quantiles <- global.quantiles + vect
    global.mean <- global.mean + quants[[i]][8] * lengths[[i]]
  } 
  global.mean <- global.mean/sum(unlist(lengths))
  global.quantiles <- global.quantiles/sum(unlist(lengths))
  output <- c(global.quantiles, global.mean)
  names(output) <- c("5%","10%","25%","50%","75%","90%","95%","Mean")
                                             
  if(type=="combine"){
    cat("\n Quantiles of the pooled data\n")
    return(output)
  }else{
    if(type=="split"){
      return(quants)
    }else{
      stop('Function argument "type" has to be either "combine" or "split"')
    }
  }                                        
}
