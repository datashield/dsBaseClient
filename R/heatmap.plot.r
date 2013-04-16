#' Generates a heatmap plot
#' @title Plots a heatmap 
#' @param xvect a numerical vector 
#' @param yvect a numerical vector
#' @return a heatmap plot.
#' @author Isaeva, J.
#' @export
#' 
heatmap.plot <- function(opals, xvect, yvect){
  
  library('fields')
  #################
  # helper function to generate the grid density table required to plot
  heatmap.plot1 <- function(xvect, yvect) {
    par(mfrow=c(1,1))
    y.min<-min(yvect)
    x.min<-min(xvect)
    y.max<-max(yvect)
    x.max<-max(xvect)
    y.range<-max(yvect)-min(yvect)
    x.range<-max(xvect)-min(xvect)
    numints<-20
    y.interval<-y.range/numints
    x.interval<-x.range/numints
    y.cuts<-seq(from=y.min,to=y.max,by=y.interval)
    y.mids<-seq(from=(y.min+y.interval/2),to=(y.max-y.interval/2),by=y.interval)
    y.cuts[numints+1]<-y.cuts[numints+1]*1.001
    x.cuts<-seq(from=x.min,to=x.max,by=x.interval)
    x.mids<-seq(from=(x.min+x.interval/2),to=(x.max-x.interval/2),by=x.interval)
    x.cuts[numints+1]<-x.cuts[numints+1]*1.001
    grid.density<-matrix(0,nrow=numints,ncol=numints)
    for(j in 1:numints){
      for(k in 1:numints){
        grid.density[j,k]<-sum(1*(yvect>=y.cuts[j] & yvect<y.cuts[j+1] & xvect >=x.cuts[k] & xvect<x.cuts[k+1]))
      }
    }
    print(length(x.mids))
    print(length(y.mids))
    # grid.density.obj<-data.frame(matrix(grid.density),as.vector(x.mids),as.vector(y.mids))
    grid.density.obj<-cbind(grid.density,x.mids,y.mids)
    return(grid.density.obj)
  }
  ##############
  
  # generate the grid density object to plot
  cally <- call("heatmap.plot.1", xvect, yvect) 
  grid.density.obj <- datashield.aggregate(opals, cally)
  
  numplots <- length(grid.density.obj)
  nrow<-1
  ncol<-1
  
  if(is.atomic(grid.density.obj)==FALSE){
    if(numplots==2)
    {
      ncol<-2
    }
    
    if(numplots==3|numplots==4)
    {
      nrow<-2
      ncol<-2
    }
    
    if(numplots==5|numplots==6)
    {
      nrow<-2
      ncol<-3
    }
    
    if(numplots>6)
    {
      nrow<-4
      ncol<-4
    }
  }
  par(mfrow=c(nrow,ncol))
  
  if(is.atomic(grid.density.obj)==FALSE)
  {
    for(j in 1:numplots)
    {
      numcols<-dim(grid.density.obj[[j]])[2]
      x<-grid.density.obj[[j]][,(numcols-1)]
      y<-grid.density.obj[[j]][,(numcols)]
      z<-grid.density.obj[[j]][,1:(numcols-2)]
      image.plot(x,y,z, col=heat.colors(50))
    }
  }
  
  if(is.atomic(grid.density.obj))
  {
    numcols<-dim(grid.density.obj)[2]
    x<-grid.density.obj[,(numcols-1)]
    y<-grid.density.obj[,(numcols)]
    z<-grid.density.obj[,1:(numcols-2)]
    library('fields')
    image.plot(x,y,z, col=heat.colors(50))
  }
}