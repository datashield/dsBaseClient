#' 
#' @title Generates a heatmap plot
#' @description Generates a heatmap plot of the pooled data or one plot for each dataset.
#' @details The function first generates a density grid and uses it to plot the graph.
#' Cells of the grid density matrix that hold a count of less than the filter set by 
#' DataSHIELD (usually 5) are considered invalid and turned into 0 to avoid potential 
#' disclosure. A message is printed to inform the user about the number of invalid cells.
#' The ranges returned by each study and used in the process of getting the grid density matrix
#' are not the exact minumum and maximum values but rather close approximates of the real
#' minimum and maximum value. This was done to reduce the risk of potential disclosure.
#' @param x a character, the name of a numerical vector
#' @param y a character, the name of a numerical vector
#' @param type a character which represents the type of graph to display. 
#' If \code{type} is set to 'combine', a combined heatmap plot displayed and 
#' if \code{type} is set to 'split', each heatmap is plotted separately.
#' @param show a character which represents where the plot should focus
#' If \code{show} is set to 'all', the ranges of the variables are used as plot limits
#' If \code{show} is set to 'zoomed', the plot is zoomed to the region where the actual data are.
#' @param numints a number of intervals for a density grid object.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources. 
#' @return a heatmap plot
#' @author Isaeva, J.; Gaye, A.
#' @export
#' @examples {
#' 
#'   # load the file that contains the login details
#'   data(logindata)
#' 
#'   # login and assign the required variables to R
#'   myvar <- list("LAB_TSC","LAB_HDL")
#'   opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#'   # Example1: generate a combined (i.e. pooled heatmap plot)
#'   ds.heatmapPlot(x='D$LAB_TSC', y='D$LAB_HDL')
#'   ds.heatmapPlot(x='D$LAB_TSC', y='D$LAB_HDL', show='zoomed')
#' 
#'   # Example2: generate a heatmapplot where each study is plotted seaparately
#'   ds.heatmapPlot(x='D$LAB_TSC', y='D$LAB_HDL', type='split')
#'   ds.heatmapPlot(x='D$LAB_TSC', y='D$LAB_HDL', type='split', show='zoomed')
#' 
#'   # Example3: generate a heatmap plot with a less dense drid
#'   ds.heatmapPlot(x='D$LAB_TSC', y='D$LAB_HDL', type='split', numints=15)
#' 
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(opals)
#' 
#' }
#'
ds.heatmapPlot <- function(x=NULL, y=NULL, type="combine", show="all", numints=20, datasources=NULL){
  
  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }
  
  if(is.null(x)){
    stop("x=NULL. Please provide the names of the 1st numeric vector!", call.=FALSE)
  }
  if(is.null(y)){
    stop("y=NULL. Please provide the names of the 2nd numeric vector!", call.=FALSE)
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
  for(i in 1:length(objects)){
    typ <- checkClass(datasources, objects[i])
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
  
  
  if(type=="combine"){
    
    # get the range from each study and produce the 'global' range
    cally <- paste("rangeDS(", x, ")") 
    x.ranges <- datashield.aggregate(datasources, as.symbol(cally))
    
    cally <- paste("rangeDS(", y, ")") 
    y.ranges <- datashield.aggregate(datasources, as.symbol(cally))
    
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
    cally <- paste0("densityGridDS(",x,",",y,",",limits=T,",",x.global.min,",",
                    x.global.max,",",y.global.min,",",y.global.max,",",numints,")")
    grid.density.obj <- datashield.aggregate(datasources, as.symbol(cally))
    
    numcol<-dim(grid.density.obj[[1]])[2]
    
    # print the number of invalid cells in each participating study
    for (i in 1:num.sources) {
      message(stdnames[i],': ', names(dimnames(grid.density.obj[[i]])[2]))
    }
    
    Global.grid.density = matrix(0, dim(grid.density.obj[[1]])[1], numcol-2)
    for (i in 1:num.sources){
      Global.grid.density = Global.grid.density + grid.density.obj[[i]][,1:(numcol-2)]
    }
    
    # prepare arguments for the plot function    
    par(mfrow=c(1,1))
    
    x<-grid.density.obj[[1]][,(numcol-1)]
    y<-grid.density.obj[[1]][,(numcol)]
    z<-Global.grid.density
    
    if (show=='all') {
      # plot a combined heatmap
      image.plot(x,y,z, xlab=x.lab, ylab=y.lab, main="Heatmap Plot of the Pooled Data")
    } else if (show=='zoomed') {
      
      # find rows and columns on the edge of the grid density object which consist only of zeros and leave only
      # one such row/column on each side
      # rows on the top
      flag = 0
      rows_top = 1
      while (flag !=1) {   # find out where non-zero elements start
        if (all(Global.grid.density[rows_top,]==0)) {
          rows_top = rows_top+1 
        } else flag=1
      }
      if (rows_top==1) {  # the first row contains non-zero elements
        dummy_top = rows_top
      } else dummy_top = rows_top-1  # leave one row at the top with only zeros
      
      # rows at the bottom
      flag = 0
      rows_bot = dim(Global.grid.density)[1]
      while (flag !=1) {   # find out where non-zero elements start
        if (all(Global.grid.density[rows_bot,]==0)) {
          rows_bot = rows_bot-1 
        } else flag=1
      }
      if (rows_bot==dim(Global.grid.density)[1]) {  # the last row contains non-zero elements
        dummy_bot = rows_bot
      } else dummy_bot = rows_bot+1  # leave one row at the bottom with only zeros
      
      # columns on the left
      flag = 0
      col_left = 1
      while (flag !=1) {   # find out where non-zero elements start
        if (all(Global.grid.density[,col_left]==0)) {
          col_left = col_left+1 
        } else flag=1
      }
      if (col_left==1) {  # the first column contains non-zero elements
        dummy_left = col_left
      } else dummy_left = col_left-1  # leave one column on the left with only zeros
      
      # columns on the right
      flag = 0
      col_right = dim(Global.grid.density)[2]
      while (flag !=1) {   # find out where non-zero elements start
        if (all(Global.grid.density[,col_right]==0)) {
          col_right = col_right-1 
        } else flag=1
      }
      if (col_right==1) {  # the first column contains non-zero elements
        dummy_right = dim(Global.grid.density)[2]
      } else dummy_right = col_right+1  # leave one column on the right with only zeros
      
      z.zoomed = Global.grid.density[dummy_top:dummy_bot, dummy_left:dummy_right]
      x.zoomed = x[dummy_top:dummy_bot]
      y.zoomed = y[dummy_left:dummy_right]
      
      # plot a combined heatmap
      image.plot(x.zoomed,y.zoomed,z.zoomed, xlab=x.lab, ylab=y.lab, main="Heatmap Plot of the Pooled Data (zoomed)")

    } else
      stop('Function argument "show" has to be either "all" or "zoomed"')
    
  } else if (type=='split') {
    
    # generate the grid density object to plot
    num_intervals=numints
    cally <- paste0("densityGridDS(",x,",",y,",",'limits=FALSE',",",'x.min=NULL',",",
                    'x.max=NULL',",",'y.min=NULL',",",'y.max=NULL',",",numints=num_intervals, ")")
    grid.density.obj <- datashield.aggregate(datasources, as.symbol(cally))
    
    numcol<-dim(grid.density.obj[[1]])[2]
    
    # print the number of invalid cells in each participating study
    for (i in 1:num.sources) {
      message(stdnames[i],': ', names(dimnames(grid.density.obj[[i]])[2]))
    }
    
    
    if(num.sources > 1){
      if((num.sources %% 2) == 0){ numr <- num.sources/2 }else{ numr <- (num.sources+1)/2}
      numc <- 2
      par(mfrow=c(numr,numc))
      for(i in 1:num.sources){
        grid <- grid.density.obj[[i]][,1:(numcol-2)]
        x<-grid.density.obj[[i]][,(numcol-1)]
        y<-grid.density.obj[[i]][,(numcol)]
        z<-grid 
        title <- paste("Heatmap Plot of ", stdnames[i], sep="")
        if (show=='all') {
          image.plot(x,y,z, xlab=x.lab, ylab=y.lab, main=title)
        } else if (show=='zoomed') {
          
          # find rows and columns on the edge of the grid density object which consist only of zeros and leave only
          # one such row/column on each side
          # rows on the top
          flag = 0
          rows_top = 1
          while (flag !=1) {   # find out where non-zero elements start
            if (all(z[rows_top,]==0)) {
              rows_top = rows_top+1 
            } else flag=1
          }
          if (rows_top==1) {  # the first row contains non-zero elements
            dummy_top = rows_top
          } else dummy_top = rows_top-1  # leave one row at the top with only zeros
          
          # rows at the bottom
          flag = 0
          rows_bot = dim(z)[1]
          while (flag !=1) {   # find out where non-zero elements start
            if (all(z[rows_bot,]==0)) {
              rows_bot = rows_bot-1 
            } else flag=1
          }
          if (rows_bot==dim(z)[1]) {  # the last row contains non-zero elements
            dummy_bot = rows_bot
          } else dummy_bot = rows_bot+1  # leave one row at the bottom with only zeros
          
          # columns on the left
          flag = 0
          col_left = 1
          while (flag !=1) {   # find out where non-zero elements start
            if (all(z[,col_left]==0)) {
              col_left = col_left+1 
            } else flag=1
          }
          if (col_left==1) {  # the first column contains non-zero elements
            dummy_left = col_left
          } else dummy_left = col_left-1  # leave one column on the left with only zeros
          
          # columns on the right
          flag = 0
          col_right = dim(z)[2]
          while (flag !=1) {   # find out where non-zero elements start
            if (all(z[,col_right]==0)) {
              col_right = col_right-1 
            } else flag=1
          }
          if (col_right==1) {  # the first column contains non-zero elements
            dummy_right = dim(z)[2]
          } else dummy_right = col_right+1  # leave one column on the right with only zeros
          
          z.zoomed = z[dummy_top:dummy_bot, dummy_left:dummy_right]
          x.zoomed = x[dummy_top:dummy_bot]
          y.zoomed = y[dummy_left:dummy_right]
          

          title <- paste("Heatmap Plot of ", stdnames[i], " (zoomed)",sep="")
          image.plot(x.zoomed,y.zoomed,z.zoomed, xlab=x.lab, ylab=y.lab, main=title)
          
        } else
          stop('Function argument "show" has to be either "all" or "zoomed"')
      }
      
    }else{
      par(mfrow=c(1,1)) 
      grid <- grid.density.obj[[1]][,1:(numcol-2)]
      x <- grid.density.obj[[1]][,(numcol-1)]
      y <- grid.density.obj[[1]][,(numcol)]
      z <- grid  
      title <- paste("Heatmap Plot of ", stdnames[1], sep="")
      if (show=='all') {
        image.plot(x,y,z, xlab=x.lab, ylab=y.lab, main=title)
      } else if (show=='zoomed') {
        
        # find rows and columns on the edge of the grid density object which consist only of zeros and leave only
        # one such row/column on each side
        # rows on the top
        flag = 0
        rows_top = 1
        while (flag !=1) {   # find out where non-zero elements start
          if (all(z[rows_top,]==0)) {
            rows_top = rows_top+1 
          } else flag=1
        }
        if (rows_top==1) {  # the first row contains non-zero elements
          dummy_top = rows_top
        } else dummy_top = rows_top-1  # leave one row at the top with only zeros
        
        # rows at the bottom
        flag = 0
        rows_bot = dim(z)[1]
        while (flag !=1) {   # find out where non-zero elements start
          if (all(z[rows_bot,]==0)) {
            rows_bot = rows_bot-1 
          } else flag=1
        }
        if (rows_bot==dim(z)[1]) {  # the last row contains non-zero elements
          dummy_bot = rows_bot
        } else dummy_bot = rows_bot+1  # leave one row at the bottom with only zeros
        
        # columns on the left
        flag = 0
        col_left = 1
        while (flag !=1) {   # find out where non-zero elements start
          if (all(z[,col_left]==0)) {
            col_left = col_left+1 
          } else flag=1
        }
        if (col_left==1) {  # the first column contains non-zero elements
          dummy_left = col_left
        } else dummy_left = col_left-1  # leave one column on the left with only zeros
        
        # columns on the right
        flag = 0
        col_right = dim(z)[2]
        while (flag !=1) {   # find out where non-zero elements start
          if (all(z[,col_right]==0)) {
            col_right = col_right-1 
          } else flag=1
        }
        if (col_right==1) {  # the first column contains non-zero elements
          dummy_right = dim(z)[2]
        } else dummy_right = col_right+1  # leave one column on the right with only zeros
        
        z.zoomed = z[dummy_top:dummy_bot, dummy_left:dummy_right]
        x.zoomed = x[dummy_top:dummy_bot]
        y.zoomed = y[dummy_left:dummy_right]
        
        

        title <- paste("Heatmap Plot of ", stdnames[1], " (zoomed)",sep="")
        image.plot(x.zoomed,y.zoomed,z.zoomed, xlab=x.lab, ylab=y.lab, main="Heatmap Plot of the Pooled Data")
        
      } else
        stop('Function argument "show" has to be either "all" or "zoomed"')
      
    }
    
  } else
    stop('Function argument "type" has to be either "combine" or "split"')
}
