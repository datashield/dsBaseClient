#'
#' @title Checks if the elements in a regression formula are defined and not empty
#' @description This is an internal function required by the client
#' function \code{ds.glm} to ensure all the variable in the LP are defined and not empty,
#' i.e. are not missing at complete.
#' @param formula an object of type formula, the lp formula
#' @param data a character, the name of an optional data frame containing the variables in 
#' in the \code{formula}.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @keywords internal
#' @return an integer 0 if check was passed and 1 if failed
#' @author Gaye, A.
#' 
geeChecks <- function(formula, data, datasources){
  
  cally <- call('complete.cases', as.symbol(data))
  opal::datashield.assign(datasources, 'Dcomplete', cally)
  
  # get names of the studies 
  stdnames <- names(datasources)
  
  # get the names of the variables in the model
  formulatext <- paste0(Reduce(paste, deparse(formula)))
  formulatext <- gsub( " ", "", formulatext, fixed=TRUE)
  formulatext <- gsub( "~", "|", formulatext, fixed=TRUE)
  formulatext <- gsub( "+", "|", formulatext, fixed=TRUE)
  formulatext <- gsub( "*", "|", formulatext, fixed=TRUE)
  variables <- unlist(strsplit(formulatext, split="|", fixed=TRUE))
  
  # checks
  colsD <- opal::datashield.aggregate(datasources, paste0("colnames(", data, ")"))[[1]]
  for(i in 1:length(variables)){
    if(is.na(as.numeric(variables[i], options(warn=-1)))){
      message(paste0("    ", variables[i], "..."))
      for(j in 1: length(datasources)){
        lengthDcomplete <- opal::datashield.aggregate(datasources[j],paste0("length(Dcomplete)"))[[1]]
        nrowD <- opal::datashield.aggregate(datasources[j], paste0("dim(", data, ")"))[[1]][1]
        if(lengthDcomplete != nrowD){
          stop(paste0("Missing value(s) in ",data,  " in ", stdnames[j] , ". Only complete datasets are allowed in GEE analysis. TIP: use 'ds.subset' to obtain a complete subset of ", data), call.=FALSE)
        }else{
          inputterms <- unlist(strsplit(deparse(variables[[i]]), "\\$", perl=TRUE))
          if(length(inputterms) > 1){
            if(!(inputterms[2] %in% colsD)){
              stop("The variable ", as.character(variables[[i]]),  " is not in the dataset ", data, " in ", stdnames[j], call.=FALSE)
            }
          }else{
            if(!(as.character(variables[[i]]) %in% colsD)){
              stop("The variable ", as.character(variables[[i]]),  " is not in the dataset ", data, " in ", stdnames[j], call.=FALSE)
            }
          }
        }
      }
    }
  }
  
}
