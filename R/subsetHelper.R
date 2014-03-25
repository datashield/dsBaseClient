#' 
#' @title Compares subset and original object sizes and eventually carries out subsetting
#' @description This is an internal function.
#' @details This function is called by the function 'ds.subset' to ensure that the requested subset
#' is not larger than the original object.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @param data a string character, the name of the dataframe or the factor vector and the range of the subset.
#' @param rows a vector of integers, the indices of the rows de extract. 
#' @param cols a vector of integers or characters; the indices of the columns to extract or the names of the columns (i.e. 
#' names of the variables to extract).
#' @return a message or the class of the object if the object has the same class in all studies.
#'
.subsetHelper <- function(datasources, data, rows, cols){

  if(!(is.null(rows) & !(is.null(cols)))){
    # if the size of the requested subset is greater than that of original inform the user and stop the process
    dims <- ds.dim(datasources, data)
    fail <- 0
    for(i in 1:length(datasources)){
      if(length(rows) > dims[[i]][1] | length(cols) > dims[[i]][2]){
        fail <- append(fail, i)     
      }
    }
    if(sum(fail) > 0){
      stop(paste0("The subset you specified is larger than ", data, " in ", paste(stdnames[fail], collapse=", "), "."))
    }else{
      # turn the vector of row indices into a character to pass the parser
      invect1 <- as.character(rows)
      invect2 <- as.character(cols)
      cally <- call('subsetDS', data, invect1, invect2)
      datashield.assign(datasources, cally)
    }
  }else{
    if(!(is.null(rows))){
      dims <- ds.dim(datasources, data)
      fail <- 0
      for(i in 1:length(datasources)){
        if(length(rows) > dims[[i]][1]){
          fail <- append(fail, i)     
        }
      }
      if(sum(fail) > 0){
        stop(paste0("The subset you specified is larger than ", data, " in ", paste(stdnames[fail], collapse=", "), "."))
      }else{
        # turn the vector of row indices into a character to pass the parser
        invect1 <- as.character(rows)
        cally <- call('subsetDS', data, invect1)
        datashield.assign(datasources, cally)
      }
    }
    if(!(is.null(cols))){
      dims <- ds.dim(datasources, data)
      fail <- 0
      for(i in 1:length(datasources)){
        if(length(rows) > dims[[i]][2]){
          fail <- append(fail, i)     
        }
      }
      if(sum(fail) > 0){
        stop(paste0("The subset you specified is larger than ", data, " in ", paste(stdnames[fail], collapse=", "), "."))
      }else{
        # turn the vector of row indices into a character to pass the parser
        invect2 <- as.character(cols)
        cally <- call('subsetDS', data, invect2)
        datashield.assign(datasources, cally)
      }
    }
  }
  
}