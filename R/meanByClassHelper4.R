#'
#' @title Gets the subset tables out of the list (i.e. unlist)
#' @description This is an internal function.
#' @details This function is called by the function 'ds.meanByClass' to obtain 'loose'
#' subset tables because the 'subsetByClass' function does not handle a table within a list.
#' @param dtsource a list of \code{\link[DSI]{DSConnection-class}} objects obtained after login. If the <datasources>
#' the default set of connections will be used: see \link[DSI]{datashield.connections_default}.
#' @param alist the name of the list that holds the final subset tables
#' @param initialtable a character the name of the table that the subset were generated from
#' @param variable a character, the variable to subset on
#' @param categories a character vector, the classes in the variables to subset on
#' @keywords internal
#' @return the 'loose' subset tables are stored on the server side
#' @author Gaye, A.
#'
meanByClassHelper4 <- function(dtsource, alist, initialtable, variable=NA, categories=NA){

  check1 <- which(unlist(strsplit(initialtable,"_")) == "INVALID")
  check2 <- which(unlist(strsplit(initialtable,"_")) == "EMPTY")

  newsubsets <- c()

  if(length(check1) > 0 | length(check2) > 0){
    for(m in 1:length(categories)){
      name2use <- paste0(initialtable,'.',variable, categories[m], "_INVALID")
      newsubsets <- append(newsubsets, name2use)
    }
  }else{
    cally <- call('namesDS', alist)
    subsetnames <- unique(unlist(DSI::datashield.aggregate(dtsource, cally)))
    for(m in 1:length(subsetnames)){
      name2use <- paste0(unlist(strsplit(paste0(initialtable,'.',subsetnames[m]), '.level_')), collapse='')
      DSI::datashield.assign(dtsource, name2use, as.symbol(paste0(alist,'$',subsetnames[m])))
      newsubsets <- append(newsubsets, name2use)
    }
  }
  return(newsubsets)
}
