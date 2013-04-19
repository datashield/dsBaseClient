#' Computes and plots histogram of the given data values.
#' @title Plots a histogram 
#' @param a vector of values for which the histogram is desired.
#' @return a histogram.
#' @author Burton, P.
#' @export
#' 
datashield.histogram <- function (opals, a) {
  
  # call the helper function and use its output
  cally <- call("histogram.1", a) 
  output.object <- datashield.aggregate(opals, cally)
  
  numsources<-length(output.object)
  
  if(numsources==1)
  {
    numr<-1
    numc<-1
  }
  
  if(numsources==2)
  {
    numr<-1
    numc<-2
  }
  
  if(numsources==3 | numsources==4)
  {
    numr<-2
    numc<-2
  }
  
  if(numsources==5 | numsources==6)
  {
    numr<-2
    numc<-3
  }
  
  if(numsources>6)
  {
    numr<-4
    numc<-4
  }
  
  par(mfrow=c(numr,numc))
  
  for(j in 1:numsources)
  {
    plot(output.object[[j]],main=c("Histogram","Study",j),xlab="Variable")
  }
}