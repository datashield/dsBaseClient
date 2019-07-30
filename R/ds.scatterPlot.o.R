#'
#' @title Generates non-disclosive scatter plots
#' @description This function uses two disclosure control methods to generate non-disclosive
#' scatter plots of two continuous variables
#' @details As the generation of a scatter plot from original data is disclosive and is not
#' permitted in DataSHIELD, this function allows the user to plot non-disclosive scatter plots.
#' If the argument \code{method} is set to 'deterministic', the server side function searches
#' for the k-1 nearest neigbours of each single data point and calculates the centroid of such k
#' points. The proximity is defined by the minimum Euclidean distances of z-score transformed data.
#' When the coordinates of all centroids are estimated the function applies scaling to expand the
#' centroids back to the dispersion of the original data. The scaling is achieved by multiplying
#' the centroids with a scaling factor that is equal to the ratio between the standard deviation of
#' the original variable and the standard deviation of the calculated centroids. The coordinates of
#' the scaled centroids are then returned to the client.The value of k in this deterministic
#' approach, is specified by the user. The suggested and default value is equal to 3 which is also
#' the suggested minimum threshold that is used to prevent disclosure which is specified in the
#' protection filter 'nfilter.kNN'. When the value of k increases, the disclosure risk decreases
#' but the utility loss increases.
#' If the argument \code{method} is set to 'probabilistic', the server side function generates a
#' random normal noise of zero mean and variance equal to 10% of the variance of each $x$ and $y$
#' variable. The noise is added to each $x$ and $y$ variable and the disturbed by the addition of
#' noise data are returned to the client. Note that the seed random number generator is fixed to a
#' specific number generated from the data and therefore the user gets the same figure every time
#' that chooses the probabilistic method in a given set of variables.
#' @param x a character, the name of a numeric vector, the x-variable.
#' @param y a character, the name of a numeric vector, the y-variable.
#' @param method a character which specifies the method that is used to generated non-disclosive
#' coordinates to be displayed in a scatter plot. If the \code{method} is set to 'deteministic'
#' (default), then the scatter plot shows the scaled centroids of each k nearest neighbours of the
#' original variables where the value of \code{k} is set by the user. If the \code{method} is set
#' to 'probabilistic', then the scatter plot shows the original data disturbed by the addition of
#' random stochastic noise. The added noise follows a normal distribution with zero mean and
#' variance equal to a percentage of the initial variance of each variable. This percentage is
#' specified by the user in the argument \code{noise}.
#' @param k the number of the nearest neghbours for which their centroid is calculated.
#' The user can choose any value for k equal to or greater than the pre-specified threshold
#' used as a disclosure control for this method and lower than the number of observations
#' minus the value of this threshold. By default the value of k is set to be equal to 3
#' (we suggest k to be equal to, or bigger than, 3). Note that the function fails if the user
#' uses the default value but the study has set a bigger threshold. The value of k is used only
#' if the argument \code{method} is set to 'deterministic'. Any value of k is ignored if the
#' argument \code{method} is set to 'probabilistic'.
#' @param noise the percentage of the initial variance that is used as the variance of the embedded
#' noise if the argument \code{method} is set to 'probabilistic'. Any value of noise is ignored if
#' the argument \code{method} is set to 'deterministic'. The user can choose any value for noise
#' equal to or greater than the pre-specified threshold 'nfilter.noise'.
#' @param type a character which represents the type of graph to display. A scatter plot for
#' combined data is generated when the \code{type} is set to 'combine'. One scatter plot for each
#' single study is generated when the \code{type} is set to 'split' (default).
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @return one or more scatter plots depending on the argument \code{type}
#' @author Demetris Avraam for DataSHIELD Development Team
#' @export
#' @examples
#' \dontrun{
#'
#'   # load the file that contains the login details
#'   data(logindata)
#' 
#'   # login to the servers
#'   opals <- opal::datashield.login(logins=logindata, assign=TRUE)
#' 
#'   # Example 1: generate a scatter plot for each study separately (the default behaviour)
#'   ds.scatterPlot.o(x='LD$PM_BMI_CONTINUOUS', y='LD$LAB_GLUC_ADJUSTED', type="split")
#'
#'   # Example 2: generate a combined scatter plot with the default deterministic method
#'   ds.scatterPlot.o(x='LD$PM_BMI_CONTINUOUS', y='LD$LAB_GLUC_ADJUSTED', k=3,
#'                    method='deterministic')
#'
#'   # Example 3: if a variable is of type factor the scatter plot is not created
#'   ds.scatterPlot.o(x='LD$PM_BMI_CATEGORICAL', y='LD$LAB_GLUC_ADJUSTED')
#'
#'   # Example 4: same as Example 2 but with k=50
#'   ds.scatterPlot.o(x='LD$PM_BMI_CONTINUOUS', y='LD$LAB_GLUC_ADJUSTED', k=50,
#'                    method='deterministic', type='combine')
#'
#'   # Example 5: same as Example 2 but with k=1740 (here we see that as k increases we have big
#'                utility loss)
#'   ds.scatterPlot.o(x='LD$PM_BMI_CONTINUOUS', y='LD$LAB_GLUC_ADJUSTED', k=1740,
#'                    method='deterministic', type='combine')
#'
#'   # Example 6: same as Example 5 but for split analysis
#'   ds.scatterPlot.o(x='LD$PM_BMI_CONTINUOUS', y='LD$LAB_GLUC_ADJUSTED', k=1740,
#'                    method='deterministic', type='split')
#'
#'   # Example 7: if k is less than the specified threshold then the scatter plot is not created
#'   ds.scatterPlot.o(x='LD$PM_BMI_CONTINUOUS', y='LD$LAB_GLUC_ADJUSTED', k=2,
#'                    method='deterministic')
#'
#'   # Example 8: generate a combined scatter plot with the probabilistic method
#'   ds.scatterPlot.o(x='LD$PM_BMI_CONTINUOUS', y='LD$LAB_GLUC_ADJUSTED', method='probabilistic',
#'                    type='combine')
#'
#'   # Example 9: generate a scatter plot with the probabilistic method for each study separately
#'   ds.scatterPlot.o(x='LD$PM_BMI_CONTINUOUS', y='LD$LAB_GLUC_ADJUSTED', method='probabilistic',
#'                    type='split')
#'
#'   # Example 10: same as Example 9 but with higher level of noise
#'   ds.scatterPlot.o(x='LD$PM_BMI_CONTINUOUS', y='LD$LAB_GLUC_ADJUSTED', method='probabilistic',
#'                    noise=0.5, type='split')
#'
#'   # Example 11: if 'noise' is less than the specified threshold then the scatter plot is not created
#'   ds.scatterPlot.o(x='LD$PM_BMI_CONTINUOUS', y='LD$LAB_GLUC_ADJUSTED', method='probabilistic',
#'                    noise=0.1, type='split')
#'
#'   # clear the Datashield R sessions and logout
#'   opal::datashield.logout(opals)
#'
#' }
#'
ds.scatterPlot.o <- function (x=NULL, y=NULL, method='deterministic', k=3, noise=0.25, type="split", datasources=NULL){

  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(x)){
    stop("Please provide the x-variable", call.=FALSE)
  }

  if(is.null(y)){
    stop("Please provide the y-variable", call.=FALSE)
  }

  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }

  # the input variable might be given as column table (i.e. D$object)
  # or just as a vector not attached to a table (i.e. object)
  # we have to make sure the function deals with each case
  objects <- c(x, y)
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
  typ.x <- checkClass(datasources, x)
  typ.y <- checkClass(datasources, y)

  # the input objects must be numeric or integer vectors
  if(typ.x != 'integer' & typ.x != 'numeric'){
    message(paste0(x, " is of type ", typ.x, "!"))
    stop("The input objects must be integer or numeric vectors.", call.=FALSE)
  }
  if(typ.y != 'integer' & typ.y != 'numeric'){
    message(paste0(y, " is of type ", typ.y, "!"))
    stop("The input objects must be integer or numeric vectors.", call.=FALSE)
  }

  # the input variable might be given as column table (i.e. D$x)
  # or just as a vector not attached to a table (i.e. x)
  # we have to make sure the function deals with each case
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
  call <- paste0("scatterPlotDS.o(", x, ",", y, ",", method.indicator, ",", k, ",", noise, ")")
  output <- opal::datashield.aggregate(datasources, call)

  pooled.points.x <- c()
  pooled.points.y <- c()
  for (i in 1:num.sources){
    pooled.points.x[[i]] <- output[[i]][[1]]
    pooled.points.y[[i]] <- output[[i]][[2]]
  }
  pooled.points.x <- unlist(pooled.points.x)
  pooled.points.y <- unlist(pooled.points.y)

  # plot and return the scatter plot depending on the argument "type"
  if(type=="combine"){
    numr <- 1
    numc <- 1
    graphics::par(mfrow=c(numr,numc))
    graphics::plot(pooled.points.x, pooled.points.y, xlab=x.lab, ylab=y.lab, main=paste0("Combined scatter plot"))
    return.message <- "Combined plot created"
    return(return.message)
  }else{
    if(type=="split"){
      # set the graph area and plot
      if(num.sources > 1){
        if((num.sources %% 2) == 0){ numr <- num.sources/2 }else{ numr <- (num.sources+1)/2}
          numc <- 2
          graphics::par(mfrow=c(numr,numc))
          scatter <- list()
        }
        for(i in 1:num.sources){
          title <- paste0("Scatter plot of ", stdnames[i])
          x <- output[[i]][[1]]
          y <- output[[i]][[2]]
          graphics::plot(x, y, xlab=x.lab, ylab=y.lab, main=title)
        }
        return.message <- "Split plot created"
        return(return.message)
    }else{
      stop('Function argument "type" has to be either "combine" or "split"')
    }
  }
}
#ds.scatterPlot.o.R
