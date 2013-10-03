#' Generates a contour plot of the given data values.
#' @title Creates a contour plot
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources. 
#' @param xvect a numerical vector 
#' @param yvect a numerical vector
#' @param type a character which represents the type of graph to display. 
#' If \code{type} is set to 'combine', a combined contour plot displayed and 
#' if \code{type} is set to 'split', each conntour is plotted separately.
#' @param numints a number of intervals for a density grid object
#' @return a contour plot
#' @author Burton, P., Isaeva, J. and Gaye, A.
#' @export
#' @examples {
#' # load the file that contains the login details
#' data(logindata)
#' 
#' # login and assign the required variables to R
#' myvar <- list("LAB_TSC","LAB_HDL")
#' opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#' # Example1: generate a combined contourplot
#' ds.contourplot(datasources=opals, quote(D$LAB_TSC), quote(D$LAB_HDL), type="combine")
#' 
#' # Example2: generate a contourplot where each study is plotted seaparately
#' ds.contourplot(datasources=opals, quote(D$LAB_TSC), quote(D$LAB_HDL), type="split")
#' 
#' # Example3: generate a contourplot with a less dense drid
#' ds.contourplot(datasources=opals, quote(D$LAB_TSC), quote(D$LAB_HDL), type="split", numints=15)
#' }
#'
ds.contourplot <- function(datasources=NULL, xvect=NULL, yvect=NULL, type='combine', numints=20){
  
  if(is.null(datasources)){
    cat("\n\n ALERT!\n")
    cat(" No valid opal object(s) provided.\n")
    cat(" Make sure you are logged in to valid opal server(s).\n")
    stop(" End of process!\n\n", call.=FALSE)
  }
  
  if(is.null(xvect)){
    cat("\n\n ALERT!\n")
    cat(" Please provide a valid numeric vector for 'xvect'\n")
    stop(" End of process!\n\n", call.=FALSE)
  }
  
  if(is.null(yvect)){
    cat("\n\n ALERT!\n")
    cat(" Please provide a valid numeric vector for 'yvec'\n")
    stop(" End of process!\n\n", call.=FALSE)
  }
  
  # labels for the x and y-axis 
  
  # the input variable might be given as column table (i.e. D$xvect)
  # or just as a vector not attached to a table (i.e. xvect)
  # we have to make sure the function deals with each case
  inputterms <- unlist(strsplit(deparse(xvect), "\\$", perl=TRUE))
  if(length(inputterms) > 1){
    x.lab <- strsplit(deparse(xvect), "\\$", perl=TRUE)[[1]][2]
  }else{
    x.lab <- deparse(xvect)
  }
  inputterms <- unlist(strsplit(deparse(yvect), "\\$", perl=TRUE))
  if(length(inputterms) > 1){
    y.lab <- strsplit(deparse(yvect), "\\$", perl=TRUE)[[1]][2]
  }else{
    y.lab <- deparse(yvect)
  }
  
  # call the function that checks the variable is available and not empty
  vars2check <- list(xvect,yvect)
  datasources <- ds.checkvar(datasources, vars2check)
  
  # name of the studies to be used in the plots' titles
  stdnames <- names(datasources)
  
  # number of studies
  num.sources <- length(datasources)
  
  # get the range from each study and produce the 'global' range
  cally <- call("range.ds", xvect) 
  x.ranges <- datashield.aggregate(datasources, cally)
  
  cally <- call("range.ds", yvect) 
  y.ranges <- datashield.aggregate(datasources, cally)
  
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
  
  x.global.min = x.range.arg[1]
  x.global.max = x.range.arg[2]
  y.global.min = y.range.arg[1]
  y.global.max = y.range.arg[2]
  
  # generate the grid density object to plot
  cally <- call("densitygrid.ds", xvect, yvect, limits=T, x.global.min, x.global.max, y.global.min, y.global.max, numints) 
  grid.density.obj <- datashield.aggregate(datasources, cally)
  
  numcol<-dim(grid.density.obj[[1]])[2]
  
  # print the number of invalid cells in each participating study
  for (i in 1:num.sources) {
    cat('\n',stdnames[i],': ', names(dimnames(grid.density.obj[[i]])[2]), '\n')
  }
  
  if(type=="combine"){
    
    Global.grid.density = matrix(0, dim(grid.density.obj[[1]])[1], numcol-2)
    for (i in 1:num.sources){
      Global.grid.density = Global.grid.density + grid.density.obj[[i]][,1:(numcol-2)]
    }
    
    # prepare arguments for the plot function    
    par(mfrow=c(1,1))
    
    x<-grid.density.obj[[1]][,(numcol-1)]
    y<-grid.density.obj[[1]][,(numcol)]
    z<-Global.grid.density
    
    # plot a combined heatmap
    contour(x,y,z, xlab=x.lab, ylab=y.lab, main="Contour Plot of the Pooled Data") 
        
  } else if (type=='split') {
    
    # define scale for plot legends
    z.min = NULL
    z.max = NULL
    
    for (i in 1:num.sources) {
      z.min = c(z.min, min(grid.density.obj[[i]][,1:(numcol-2)]))
      z.max = c(z.max, max(grid.density.obj[[i]][,1:(numcol-2)]))
    }
    
    z.global.min = min(z.min)
    z.global.max = max(z.max)
    
    if(num.sources > 1){
      if((num.sources %% 2) == 0){ numr <- num.sources/2 }else{ numr <- (num.sources+1)/2}
      numc <- 2
      par(mfrow=c(numr,numc))
      for(i in 1:num.sources){
        grid <- grid.density.obj[[i]][,1:(numcol-2)]
        x<-grid.density.obj[[i]][,(numcol-1)]
        y<-grid.density.obj[[i]][,(numcol)]
        z<-grid 
        title <- paste("Contour Plot of ", stdnames[i], sep="")
        contour(x,y,z, xlab=x.lab, ylab=y.lab, zlim=c(z.global.min,z.global.max), main=title)
      }
    }else{
      par(mfrow=c(1,1)) 
      grid <- grid.density.obj[[1]][,1:(numcol-2)]
      x <- grid.density.obj[[1]][,(numcol-1)]
      y <- grid.density.obj[[1]][,(numcol)]
      z <- grid  
      title <- paste("Contour Plot of ", stdnames[1], sep="")
      contour(x,y,z, xlab=x.lab, ylab=y.lab, zlim=c(z.global.min,z.global.max), main=title)   
    }    
  } else
    stop('Function argument "type" has to be either "combine" or "split"')
  
  
#   numplots <- length(grid.density.obj)
#   nrow<-1
#   ncol<-1
#   
#   if(is.atomic(grid.density.obj)==FALSE){
#     if(numplots==2)
#     {
#       ncol<-2
#     }
#     
#     if(numplots==3|numplots==4)
#     {
#       nrow<-2
#       ncol<-2
#     }
#     
#     if(numplots==5|numplots==6)
#     {
#       nrow<-2
#       ncol<-3
#     }
#     
#     if(numplots>6)
#     {
#       nrow<-4
#       ncol<-4
#     }
#   }
#   par(mfrow=c(nrow,ncol))
#   
#   if(is.atomic(grid.density.obj)==FALSE)
#   {
#     for(j in 1:numplots)
#     {
#       numcols<-dim(grid.density.obj[[j]])[2]
#       x<-grid.density.obj[[j]][,(numcols-1)]
#       y<-grid.density.obj[[j]][,(numcols)]
#       z<-grid.density.obj[[j]][,1:(numcols-2)]
#       contour(x,y,z) 
#       
#     }
#   }
#   
#   
#   if(is.atomic(grid.density.obj))
#   {
#     numcols<-dim(grid.density.obj)[2]
#     x<-grid.density.obj[,(numcols-1)]
#     y<-grid.density.obj[,(numcols)]
#     z<-grid.density.obj[,1:(numcols-2)]
#     contour(x,y,z)
#   }
}
