#' Generates a heatmap plot
#' @title Plots a heatmap 
#' @param xvect a numerical vector 
#' @param yvect a numerical vector
#' @return a heatmap plot.
#' @author Isaeva, J.
#' @export
#' 
datashield.heatmap.plot <- function(opals, xvect, yvect){
  
  # generate the grid density object to plot
  cally <- call("density.grid.ag", xvect, yvect) 
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
      density.matrix<-as.matrix(data.frame(as.vector(grid.density.obj[j])))
      numrows<-dim(density.matrix)[1]
      numcols<-dim(density.matrix)[2]-2
      xlabels<-round(density.matrix[,(numcols+1)],2)
      ylabels<-round(density.matrix[,(numcols+2)],2)
      
      density.matrix<-density.matrix[,1:20]
      
      
      heatmap(density.matrix,Rowv=NA,Colv=NA,labRow=ylabels,labCol=xlabels)
      
    }
  }
  
  
  if(is.atomic(grid.density.obj))
  {
    for(j in 1:numplots)
    {
      density.matrix<-as.matrix(data.frame(as.vector(grid.density.obj[j])))
      numrows<-dim(density.matrix)[1]
      numcols<-dim(density.matrix)[2]-2
      xlabels<-round(density.matrix[,(numcols+1)],2)
      ylabels<-round(density.matrix[,(numcols+2)],2)
      
      density.matrix<-density.matrix[,1:20]
      
      
      heatmap(density.matrix,Rowv=NA,Colv=NA,labRow=ylabels,labCol=xlabels)
      
    }
  }
}