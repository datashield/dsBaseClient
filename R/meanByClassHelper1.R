#' 
#' @title Generates subset tables
#' @description This is an internal function.
#' @details This function is called by the function 'ds.meanByClass' to break down the initial
#' table by the specified categorical variables.
#' @param dtsource an opal object(s) obtained after login in to opal servers;
#' @param tables a character vector, the tables to breakdown
#' @param variable a character, the variable to subset on
#' @param categories a character vector, the classes in the variables to subset on
#' @return a character the names of the new subset tables
#'
.meanByClassHelper1 <- function(dtsource, tables, variable, categories){
  
  newtablenames <- c()
  for(i in 1:length(tables)){
    # check if the vector to subset by is not empty(i.e. NAs only) as this might cause ds.subclass to crash
    ds.asNumeric(dtsource, paste0(tables[i], "$", variable), 'vectnumeric')
    check0 <- unlist(datashield.aggregate(dtsource, quote(isNA.ds(vectnumeric))))
    if(check0){
      dt2process <- paste0(tables[i], "_INVALID")
    }else{ 
      dt2process <- tables[i]
    }

    check1 <- which(unlist(strsplit(dt2process,"_")) == "INVALID")
    check2 <- which(unlist(strsplit(dt2process,"_")) == "EMPTY")
    if(length(check1) > 0 | length(check2) > 0){ 
      newtablenames <- append(newtablenames, dsbaseclient:::.meanByClassHelper4(dtsource, paste0('holder',i), dt2process, variable, categories))
    }else{
      ds.subclass(dtsource, paste0('holder',i), dt2process, variable)
      newtablenames <- append(newtablenames, dsbaseclient:::.meanByClassHelper4(dtsource, paste0('holder',i), dt2process))
    }
  }
  
  return(newtablenames)
}  
