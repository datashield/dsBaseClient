#' Generates a combine 2D table across all the datasets
#' @title Creates a 2D table
#' @param opals character strings that represent the URL of the servers where 
#' the study datasets are stored.
#' @param a a numerical vector 
#' @param a a numerical vector
#' @return a 2D table
#' @author Burton, P.
#' @export

datashield.table.2d <- function (opals, a, b){
  
  
  # use the helper function to create the 2d tables to combine
  cally <- call("table.2d", a, b) 
  
  output.object <- vector("list", length(opals))
  for (i in 1:length(opals)){
    output.object[[i]] <- datashield.aggregate(opals[[i]], cally)
  }
  
  num.sources <- length(output.object)
  table.valid<-rep(NA,num.sources)
  
  for(j in 1:num.sources)
  {
    table.valid[j]<-(sum(output.object[[j]])>=0)
  }
  cat("\n\n\nSOURCES WITH VALID DATA\ni.e. ABSENCE OF ANY CELLS WITH 1-4 OBSERVATIONS\n")
  print(cbind(1:num.sources,table.valid))
  
  tab.final<-as.matrix(output.object[[1]])*0
  
  
  for(j in 1:num.sources)
  {
    if(table.valid[j]==1)
    {
      tab.final<-tab.final+as.matrix(output.object[[j]])
    }
  }
  
  
  #print(tab.final)
  
  total.count<-sum(tab.final)
  
  numrows<-dim(tab.final)[1]
  numcols<-dim(tab.final)[2]
  
  table.combination.valid<-1
  
  for(j in 1:numrows)
  {
    for(m in 2:num.sources)
    {
      if(dimnames(output.object[[m-1]])$Var1[j]!= dimnames(output.object[[m]])$Var1[j])
      {table.combination.valid<-0}
    }
  }
  
  for(j in 1:numcols)
  {
    for(m in 2:num.sources)
    {
      if(dimnames(output.object[[m-1]])$Var2[j]!= dimnames(output.object[[m]])$Var2[j])
      {table.combination.valid<-0}
    }
  }
  
  col.sum.vect<-rep(1,numrows)
  row.sum.vect<-rep(1,numcols)
  
  col.tots<-t(tab.final)%*%col.sum.vect
  row.tots<- tab.final%*%row.sum.vect
  
  #print(col.tots)
  #print(row.tots)
  
  tab.final.counts<-tab.final
  tab.final.col.perc<-tab.final
  tab.final.row.perc<-tab.final
  tab.final.global.perc<-tab.final
  
  for(j in 1:numcols)
  {
    tab.final.col.perc[,j]<-tab.final.col.perc[,j]/col.tots[j]
  }
  
  for(k in 1:numrows)
  {
    tab.final.row.perc[k,]<-tab.final.row.perc[k,]/row.tots[k]
  }
  
  tab.final.global.perc<-tab.final/total.count
  
  if(table.combination.valid<1)
  {
    cat("\n\nTABLES HAVE DIFFERENT CATEGORIES CANNOT COMBINE\n\n")
  }
  # added by Amadou on 16.04.14 to get hold of the counts table #############################
  list2return <- list(final.counts=tab.final.counts, final.col.perc=tab.final.col.perc,
                      final.row.perc=tab.final.row.perc, final.global.perc=tab.final.global.perc)
  return(list2return) 
  ###########################################################################################	
}
