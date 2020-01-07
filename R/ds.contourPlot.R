#' 
#' @title Generates a contour plot
#' @description Generates a countour plot of the pooled data or one plot for each dataset.
#' @details The function first generates a density grid and uses it to plot the graph.
#' Cells of the grid density matrix that hold a count of less than the filter set by 
#' DataSHIELD (usually 5) are considered invalid and turned into 0 to avoid potential 
#' disclosure. A message is printed to inform the user about the number of invalid cells.
#' The ranges returned by each study and used in the process of getting the grid density matrix
#' are not the exact minumum and maximum values but rather close approximates of the real
#' minimum and maximum value. This was done to reduce the risk of potential disclosure.
#' @param x a character, the name of a numerical vector.
#' @param y a character, the name of a numerical vector.
#' @param type a character which represents the type of graph to display. 
#' If \code{type} is set to 'combine', a combined contour plot displayed and 
#' if \code{type} is set to 'split', each conntour is plotted separately.
#' @param show a character which represents where the plot should focus.
#' If \code{show} is set to 'all', the ranges of the variables are used as plot limits.
#' If \code{show} is set to 'zoomed', the plot is zoomed to the region where the actual data are.
#' @param numints a number of intervals for a density grid object.
#' @param method a character which defines which contour will be created. If \code{method}
#' is set to 'smallCellsRule' (default option), the contour plot of the actual variables is
#' created but grids with low counts are replaced with grids with zero counts. If \code{method} is
#' set to 'deterministic' the contour of the scaled centroids of each k nearest neighbours of the
#' original variables is created, where the value of \code{k} is set by the user. If the
#' \code{method} is set to 'probabilistic', then the contour of 'noisy' variables is generated. The
#' added noise follows a normal distribution with zero mean and variance equal to a percentage of
#' the initial variance of each input variable. This percentage is specified by the user in the
#' argument \code{noise}.
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
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources. 
#' @return a contour plot
#' @author Julia Isaeva, Amadou Gaye, Paul Burton, Demetris Avraam for DataSHIELD Development Team
#' @export
#' @examples
#' \dontrun{
#'
#'   # load the file that contains the login details
#'   data(logindata)
#' 
#'   # login and assign specific variables(s)
#'   # (by default the assigned dataset is a dataframe named 'D')
#'   opals <- datashield.login(logins=logindata,assign=TRUE)
#' 
#'   # Example 1: Plot a combined (default behaviour) contour plot of the variables 'LAB_TSC' 
#'   # and 'LAB_HDL' using the method 'LowCountsRule' (default method) that applies a stochastic noise 
#'   # in the extreme values of the variables' range.
#'   ds.contourPlot(x='D$LAB_TSC', y='D$LAB_HDL')
#'   
#'   # Example 2: the same as example 1
#'   ds.contourPlot(x='D$LAB_TSC', y='D$LAB_HDL', method="smallCellsRule", type='combine')
#'   
#'   # Example 3: similar as example 2 but for type='split'
#'   ds.contourPlot(x='D$LAB_TSC', y='D$LAB_HDL', method="smallCellsRule", type='split')
#'   
#'   # Example 4: Plot a combined (default behaviour) contour plot of the variables 'LAB_TSC' 
#'   # and 'LAB_HDL' using the method 'deterministic' that plots the exact contour plot of the
#'   # centroids of each 3 (default number) nearest neighbours. 
#'   ds.contourPlot(x='D$LAB_TSC', y='D$LAB_HDL', method="deterministic")
#'   
#'   # Example 5: the same as example 4
#'   ds.contourPlot(x='D$LAB_TSC', y='D$LAB_HDL', method="deterministic", k=3, type='combine')
#'   
#'   # Example 6: similar as example 5 for type='split'
#'   ds.contourPlot(x='D$LAB_TSC', y='D$LAB_HDL', method="deterministic", k=3, type='split')
#'   
#'   # Example 7: similar as example 6 for k=7
#'   ds.contourPlot(x='D$LAB_TSC', y='D$LAB_HDL', method="deterministic", k=7, type='split')
#'   
#'   # Example 8: similar as example 7 for numints=40
#'   ds.contourPlot(x='D$LAB_TSC', y='D$LAB_HDL', numints=40, method="deterministic", k=7,
#'                  type='split')
#'   
#'   # Example 9: Plot a combined (default behaviour) contour plot of the variables 'LAB_TSC' 
#'   # and 'LAB_HDL' using the method 'probabilistic' that plots the exact contour plot of the
#'   # noisy data
#'   ds.contourPlot(x='D$LAB_TSC', y='D$LAB_HDL', method="probabilistic")
#'   
#'   # Example 10: the same as example 9
#'   ds.contourPlot(x='D$LAB_TSC', y='D$LAB_HDL', method="probabilistic", noise=0.25, type='combine')
#'   
#'   # Example 11: the same as example 10 but for bigger level of noise
#'   ds.contourPlot(x='D$LAB_TSC', y='D$LAB_HDL', method="probabilistic", noise=2, type='combine')
#'   
#'   # Example 12: the same as example 11 but for type='split'
#'   ds.contourPlot(x='D$LAB_TSC', y='D$LAB_HDL', method="probabilistic", noise=2, type='split')
#'   
#'   # Example 13: if any of the input variables is a factor then the function fails
#'   ds.contourPlot(x='D$LAB_TSC', y='D$GENDER')
#' 
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(opals)
#' 
#' }
#'
ds.contourPlot <- function(x=NULL, y=NULL, type='combine', show='all', numints=20, method="smallCellsRule", k=3, noise=0.25, datasources=NULL){
  
  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }
  
  if(is.null(x)){
    stop("x=NULL. Please provide the names of two numeric vectors!", call.=FALSE)
  }
  if(is.null(y)){
    stop("y=NULL. Please provide the names of two numeric vectors!", call.=FALSE)
  }
  
  # the argument method must be either "smallCellsRule" or "deterministic" or "probabilistic"
  if(method != 'smallCellsRule' & method != 'deterministic' & method != 'probabilistic'){
    stop('Function argument "method" has to be either "smallCellsRule" or "deterministic" or "probabilistic"', call.=FALSE)
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
  
  # if the method is set to 'deterministic' or 'probabilistic' call the server-side function 
  # heatmapPlotDS that generates the anonymous data. NOTE is the same server-side function that
  # is used by the ds.heatmapPlot function
  if (method=="deterministic"){
    
    method.indicator <- 1
    
    # call the server-side function that generates the x and y coordinates of the centroids
    cally <- paste0("heatmapPlotDS(", x, ",", y, ",", k, ",", noise, ",", method.indicator, ")")
    anonymous.data <- opal::datashield.aggregate(datasources, cally)
    
    pooled.points.x <- c()
    pooled.points.y <- c()
    for (i in 1:num.sources){
      pooled.points.x[[i]] <- anonymous.data[[i]][[1]]
      pooled.points.y[[i]] <- anonymous.data[[i]][[2]]
    }  
  }
  
  if (method=="probabilistic"){
    
    method.indicator <- 2
    
    # call the server-side function that generates the x and y coordinates of the anonymous.data
    cally <- paste0("heatmapPlotDS(", x, ",", y, ",", k, ",", noise, ",", method.indicator, ")")
    anonymous.data <- opal::datashield.aggregate(datasources, cally)
    
    pooled.points.x <- c()
    pooled.points.y <- c()
    for (i in 1:num.sources){
      pooled.points.x[[i]] <- anonymous.data[[i]][[1]]
      pooled.points.y[[i]] <- anonymous.data[[i]][[2]]
    }  
  }
  
  if(type=="combine"){
    
    if(method=='smallCellsRule'){   
 
    # get the range from each study and produce the 'global' range
    cally <- paste0("rangeDS(", x, ")") 
    x.ranges <- opal::datashield.aggregate(datasources, as.symbol(cally))
    
    cally <- paste0("rangeDS(", y, ")") 
    y.ranges <- opal::datashield.aggregate(datasources, as.symbol(cally))
    
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
    cally <- paste0("densityGridDS(",x,",",y,",",limits=T,",",x.global.min,",",
                    x.global.max,",",y.global.min,",",y.global.max,",",numints, ")")
    grid.density.obj <- opal::datashield.aggregate(datasources, as.symbol(cally))
    
    numcol <- dim(grid.density.obj[[1]])[2]
    
    # print the number of invalid cells in each participating study
    for (i in 1:num.sources) {
      message(stdnames[i],': ', names(dimnames(grid.density.obj[[i]])[2]))
    }
    
    Global.grid.density <- matrix(0, dim(grid.density.obj[[1]])[1], numcol-2)
    for (i in 1:num.sources){
      Global.grid.density <- Global.grid.density + grid.density.obj[[i]][,1:(numcol-2)]
    }

  }else{

    if (method=="deterministic" | method=="probabilistic"){ 

        xvect <- unlist(pooled.points.x)
        yvect <- unlist(pooled.points.y)
 
        # generate the grid density object to plot
        y.min <- min(yvect)
        x.min <- min(xvect)
        y.max <- max(yvect)
        x.max <- max(xvect)
        
        y.range <- y.max - y.min
        x.range <- x.max - x.min
   
        y.interval <- y.range / numints
        x.interval <- x.range / numints
 
        y.cuts <- seq(from = y.min, to = y.max, by = y.interval)
        y.mids <- seq(from = (y.min + y.interval/2), to = (y.max - y.interval/2), by = y.interval)
        y.cuts[numints+1] <- y.cuts[numints+1] * 1.001
   
        x.cuts <- seq(from = x.min, to = x.max, by = x.interval)
        x.mids <- seq(from = (x.min + x.interval/2), to = (x.max - x.interval/2), by = x.interval)
        x.cuts[numints+1] <- x.cuts[numints+1] * 1.001
   
        grid.density <- matrix(0, nrow=numints, ncol=numints)
   
        for(j in 1:numints){
          for(k in 1:numints){
            grid.density[j,k] <- sum(1*(yvect >= y.cuts[k] & yvect < y.cuts[k+1] & xvect >= x.cuts[j] & xvect < x.cuts[j+1]), na.rm=TRUE)     
          }
        }
        grid.density.obj <- list()
        grid.density.obj[[1]] <- cbind(grid.density,x.mids,y.mids)
   
        numcol <- dim(grid.density.obj[[1]])[2]
     
        Global.grid.density <- grid.density
   
      }
    }  
     
    # prepare arguments for the plot function    
    graphics::par(mfrow=c(1,1))
    
    x <- grid.density.obj[[1]][,(numcol-1)]
    y <- grid.density.obj[[1]][,(numcol)]
    z <- Global.grid.density
    
    if (show=='all') {
      # plot a combined contour plot
      graphics::contour(x,y,z, xlab=x.lab, ylab=y.lab, main="Contour Plot of the Pooled Data") 
    } else if (show=='zoomed') {
      
      # find rows and columns on the edge of the grid density object which consist only of zeros and leave only
      # one such row/column on each side
      # rows on the top
      flag <- 0
      rows_top <- 1
      while (flag !=1) {   # find out where non-zero elements start
        if (all(Global.grid.density[rows_top,]==0)) {
          rows_top <- rows_top + 1 
        }else{
          flag <- 1
        }  
      }
      if (rows_top==1) {  # the first row contains non-zero elements
        dummy_top <- rows_top
      }else{
        dummy_top <- rows_top - 1  # leave one row at the top with only zeros
      } 
      
      # rows at the bottom
      flag <- 0
      rows_bot <- dim(Global.grid.density)[1]
      while (flag !=1) {   # find out where non-zero elements start
        if (all(Global.grid.density[rows_bot,]==0)){
          rows_bot <- rows_bot - 1 
        }else{
          flag <- 1
        }  
      }
      if (rows_bot==dim(Global.grid.density)[1]) {  # the last row contains non-zero elements
        dummy_bot <- rows_bot
      }else{
        dummy_bot <- rows_bot + 1  # leave one row at the bottom with only zeros
      }
      
      # columns on the left
      flag <- 0
      col_left <- 1
      while (flag !=1) {   # find out where non-zero elements start
        if (all(Global.grid.density[,col_left]==0)) {
          col_left <- col_left + 1 
        }else{
          flag <- 1
        }  
      }
      if (col_left==1) {  # the first column contains non-zero elements
        dummy_left <- col_left
      }else{
        dummy_left <- col_left - 1  # leave one column on the left with only zeros
      }
      
      # columns on the right
      flag <- 0
      col_right <- dim(Global.grid.density)[2]
      while (flag !=1) {   # find out where non-zero elements start
        if (all(Global.grid.density[,col_right]==0)) {
          col_right <- col_right - 1 
        }else{
          flag <- 1
        }  
      }
      if (col_right==1) {  # the first column contains non-zero elements
        dummy_right <- dim(Global.grid.density)[2]
      }else{
        dummy_right <- col_right + 1  # leave one column on the right with only zeros
      }
      
      z.zoomed <- Global.grid.density[dummy_top:dummy_bot, dummy_left:dummy_right]
      x.zoomed <- x[dummy_top:dummy_bot]
      y.zoomed <- y[dummy_left:dummy_right]
      
      # plot a combined contour plot
      graphics::contour(x.zoomed,y.zoomed,z.zoomed, xlab=x.lab, ylab=y.lab, main="Contour Plot of the Pooled Data (zoomed)")
    }else{
      stop('Function argument "show" has to be either "all" or "zoomed"')
    }
    
  } else if (type=='split') {

   if(method=="smallCellsRule"){
     # generate the grid density object to plot
     num_intervals <- numints
     cally <- paste0("densityGridDS(",x,",",y,",",'limits=FALSE',",",'x.min=NULL',",",
                    'x.max=NULL',",",'y.min=NULL',",",'y.max=NULL',",",numints=num_intervals, ")")
     grid.density.obj <- opal::datashield.aggregate(datasources, as.symbol(cally))
     numcol <- dim(grid.density.obj[[1]])[2]
   }
    
   if (method=="deterministic" | method=="probabilistic"){
     
    grid.density.obj <- list()
    for (i in 1:num.sources){
       xvect <- unlist(anonymous.data[[i]][[1]])
       yvect <- unlist(anonymous.data[[i]][[2]])
 
       # generate the grid density object to plot    
       y.min <- min(yvect)
       x.min <- min(xvect)
       y.max <- max(yvect)
       x.max <- max(xvect)
        
       y.range <- y.max-y.min
       x.range <- x.max-x.min
   
       y.interval <- y.range/numints
       x.interval <- x.range/numints
 
       y.cuts <- seq(from = y.min, to = y.max, by = y.interval)
       y.mids <- seq(from = (y.min + y.interval/2), to = (y.max - y.interval/2), by = y.interval)
       y.cuts[numints+1] <- y.cuts[numints+1] * 1.001
   
       x.cuts <- seq(from = x.min, to = x.max, by = x.interval)
       x.mids <- seq(from = (x.min + x.interval/2), to = (x.max - x.interval/2), by = x.interval)
       x.cuts[numints+1] <- x.cuts[numints+1] * 1.001
   
       grid.density <- matrix(0, nrow=numints, ncol=numints)
   
       for(j in 1:numints){
         for(k in 1:numints){
           grid.density[j,k] <- sum(1*(yvect >= y.cuts[k] & yvect < y.cuts[k+1] & xvect >= x.cuts[j] & xvect < x.cuts[j+1]), na.rm=TRUE)     
         }
       }
       grid.density.obj[[i]] <- cbind(grid.density, x.mids, y.mids)
        
       numcol <- dim(grid.density.obj[[i]])[2]
     }
   }
  
    # print the number of invalid cells in each participating study
    for (i in 1:num.sources) {
      message(stdnames[i],': ', names(dimnames(grid.density.obj[[i]])[2]))
    } 
    
    if(num.sources > 1){
      if((num.sources %% 2) == 0){ numr <- num.sources/2 }else{ numr <- (num.sources+1)/2}
      numc <- 2
      graphics::par(mfrow=c(numr,numc))
      for(i in 1:num.sources){
        grid <- grid.density.obj[[i]][,1:(numcol-2)]
        x<-grid.density.obj[[i]][,(numcol-1)]
        y<-grid.density.obj[[i]][,(numcol)]
        z<-grid 
        title <- paste("Contour Plot of ", stdnames[i], sep="")
        if (show=='all') {
          graphics::contour(x,y,z, xlab=x.lab, ylab=y.lab, main=title)
        } else if (show=='zoomed') {
          
          # find rows and columns on the edge of the grid density object which consist only of zeros and leave only
          # one such row/column on each side
          # rows on the top
          flag <- 0
          rows_top <- 1
          while (flag !=1) {   # find out where non-zero elements start
            if (all(z[rows_top,]==0)) {
              rows_top <- rows_top+1 
            }else{
              flag <- 1
            }  
          }
          if (rows_top==1) {  # the first row contains non-zero elements
            dummy_top <- rows_top
          }else{
            dummy_top <- rows_top - 1  # leave one row at the top with only zeros
          }
          
          # rows at the bottom
          flag <- 0
          rows_bot <- dim(z)[1]
          while (flag !=1) {   # find out where non-zero elements start
            if (all(z[rows_bot,]==0)) {
              rows_bot <- rows_bot - 1 
            }else{
              flag <- 1
            }  
          }
          if (rows_bot==dim(z)[1]) {  # the last row contains non-zero elements
            dummy_bot <- rows_bot
          }else{
            dummy_bot <- rows_bot + 1  # leave one row at the bottom with only zeros
          }
          
          # columns on the left
          flag <- 0
          col_left <- 1
          while (flag !=1) {   # find out where non-zero elements start
            if (all(z[,col_left]==0)) {
              col_left <- col_left + 1 
            }else{
              flag <- 1
            }
          }
          if (col_left==1) {  # the first column contains non-zero elements
            dummy_left <- col_left
          }else{
            dummy_left <- col_left - 1  # leave one column on the left with only zeros
          }
          
          # columns on the right
          flag <- 0
          col_right <- dim(z)[2]
          while (flag !=1) {   # find out where non-zero elements start
            if (all(z[,col_right]==0)) {
              col_right <- col_right - 1 
            }else{
              flag <- 1
            }  
          }
          if (col_right==1) {  # the first column contains non-zero elements
            dummy_right <- dim(z)[2]
          }else{
            dummy_right <- col_right + 1  # leave one column on the right with only zeros
          }
          
          z.zoomed <- z[dummy_top:dummy_bot, dummy_left:dummy_right]
          x.zoomed <- x[dummy_top:dummy_bot]
          y.zoomed <- y[dummy_left:dummy_right]
          
          title <- paste("Contour Plot of ", stdnames[i], " (zoomed)",sep="")
          graphics::contour(x.zoomed,y.zoomed,z.zoomed, xlab=x.lab, ylab=y.lab, main=title)
          
        }else{
          stop('Function argument "show" has to be either "all" or "zoomed"')
        }
        
      }
    }else{
      graphics::par(mfrow=c(1,1)) 
      grid <- grid.density.obj[[1]][,1:(numcol-2)]
      x <- grid.density.obj[[1]][,(numcol-1)]
      y <- grid.density.obj[[1]][,(numcol)]
      z <- grid  
      title <- paste("Contour Plot of ", stdnames[1], sep="")
      if(show=='all'){
        graphics::contour(x,y,z, xlab=x.lab, ylab=y.lab, main=title)
      } else if (show=='zoomed') {
        
      # find rows and columns on the edge of the grid density object which consist only of zeros and leave only
      # one such row/column on each side rows on the top
      flag <- 0
      rows_top <- 1
      while(flag !=1){   # find out where non-zero elements start
        if(all(z[rows_top,]==0)){
          rows_top <- (rows_top + 1) 
        }else{
          flag <- 1
        }	
      }
      if(rows_top==1){  # the first row contains non-zero elements
        dummy_top <- rows_top
      }else{
        dummy_top <- (rows_top - 1)  # leave one row at the top with only zeros
      }
		
      # rows at the bottom
      flag <- 0
      rows_bot <- dim(z)[1]
      while(flag !=1){   # find out where non-zero elements start
        if(all(z[rows_bot,]==0)){
          rows_bot <- (rows_bot - 1) 
        }else{
		  flag <- 1
	    }		
      }
      if(rows_bot==dim(z)[1]){  # the last row contains non-zero elements
        dummy_bot <- rows_bot
      }else{
        dummy_bot <- (rows_bot + 1)  # leave one row at the bottom with only zeros
      }
		
        # columns on the left
        flag <- 0
        col_left <- 1
        while(flag !=1){   # find out where non-zero elements start
          if(all(z[,col_left]==0)){
            col_left <- (col_left + 1) 
          }else{
            flag <- 1
		  }	
        }
        if(col_left==1){  # the first column contains non-zero elements
          dummy_left <- col_left
        }else{
          dummy_left <- (col_left - 1)  # leave one column on the left with only zeros
        }
		
        # columns on the right
        flag <- 0
        col_right <- dim(z)[2]
        while (flag !=1) {   # find out where non-zero elements start
          if (all(z[,col_right]==0)) {
            col_right <- (col_right - 1) 
          }else{
            flag <- 1
		  }
        }
        if(col_right==1){  # the first column contains non-zero elements
          dummy_right <- dim(z)[2]
        }else{
          dummy_right <- (col_right + 1)  # leave one column on the right with only zeros
        }
		
        z.zoomed <- z[dummy_top:dummy_bot, dummy_left:dummy_right]
        x.zoomed <- x[dummy_top:dummy_bot]
        y.zoomed <- y[dummy_left:dummy_right]
         
        title <- paste("Contour Plot of ", stdnames[1], " (zoomed)",sep="")
        graphics::contour(x.zoomed,y.zoomed,z.zoomed, xlab=x.lab, ylab=y.lab, main="Contour Plot of the Pooled Data")
        
      }else{
        stop('Function argument "show" has to be either "all" or "zoomed"')
      }
	}    
  }else{
    stop('Function argument "type" has to be either "combine" or "split"')
  }
  
}
