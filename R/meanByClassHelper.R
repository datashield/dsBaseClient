#' 
#' @title Generates new names from subset tables names
#' @description This is an internal function.
#' @details This function is called by the function 'ds.meanByClass' to ensure the names of the subset are 
#' identical in the different studies. After running the function 'ds.subclass' invalid and empty subsets
#' have a suffixe '_INVALID' or '_EMPTY' on their names; this is required to inform user of failed subsets but
#' when poses problem when computing mean and SD as the names may not be uniform across the studies whilst 
#' uniformity of names across studies is required in any datashield process.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @param listname a character, name of the list that holds the subset tables 
#' @param covarname a character, the name of of the categorical variable to subset on.
#' @param classes a character vector, the lebbels of the categorical variable. 
#' @param tablename a character, the name of the table the subsets in 'listname' where generated from
#' @return a character vector, the new names of the subset tables
#'
.meanByClassHelper <- function(datasources, tablename, listname, covarname, classes){
  subnames <- unique(unlist(ds.names(datasources, listname)))
  names2use <- c()
  for(j in 1:length(classes)){
    ds.assign(datasources, paste0(tablename, ".",covarname,'.level_', classes[j]), paste0(listname,'$',subnames[j]))
    names2use <- append(names2use,  paste0(tablename, ".",covarname,'.level_', classes[j]))
  }
  return(names2use)
}