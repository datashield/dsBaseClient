#' 
#' @title Gets the subset tables out of the list (i.e. unlist)
#' @description This is an internal function.
#' @details This function is called by the function 'ds.meanByClass' to obtain 'loose'
#' subset tables because the 'subclass' function does not handle a table within a list.
#' @param dtsource a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @param alist the name of the list that holds the final subset tables
#' @param initialtable a character the name of the table that the subset were generated from
#' @param variable a character, the variable to subset on
#' @param categories a character vector, the classes in the variables to subset on
#' @return the 'loose' subset tables are stored on the server side
#'
.meanByClassHelper4 <- function(dtsource, alist, initialtable, variable=NA, categories=NA){
  
  check1 <- which(unlist(strsplit(initialtable,"_")) == "INVALID")
  check2 <- which(unlist(strsplit(initialtable,"_")) == "EMPTY")
  
  newsubsets <- c()
  
  if(length(check1) > 0 | length(check2) > 0){ 
    for(m in 1:length(categories)){
      newsubsets <- append(newsubsets, paste0(initialtable,'.',variable,".level_", categories[m], "_INVALID"))
    }
  }else{
    subsetnames <- unique(unlist(ds.names(dtsource, alist)))
    for(m in 1:length(subsetnames)){
      datashield.assign(dtsource, paste0(initialtable,'.',subsetnames[m]), as.symbol(paste0(alist,'$',subsetnames[m])))
      newsubsets <- append(newsubsets, paste0(initialtable,'.',subsetnames[m]))
    }
  }
  return(newsubsets)    
}