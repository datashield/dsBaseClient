#'
#' @title Checks if the elements in the glm model have the right characteristics
#' @description This is an internal function required by the client function \code{ds.glm} 
#' to verify all the variables and ensure the process does not halt inadvertanly.
#' @details the variables are checked to ensure they are defined, not empty (i.e. are not missing 
#' at complete) and evantually (if 'offset' or 'weights') are of 'numeric' with non negative value 
#' (if 'weights').
#' @param formula a character, a regression formula given as a string character
#' @param data a character, the name of an optional data frame containing the variables in 
#' in the \code{formula}.
#' @param offset  null or a numreric vector that can be used to specify an a priori known component to be 
#' included in the linear predictor during fitting.
#' @param weights  a character, the name of an optional vector of 'prior weights' to be used in the fitting 
#' process. Should be NULL or a numeric vector.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @keywords internal
#' @return an integer 0 if check was passed and 1 if failed
#' @author Gaye, A.
#' 
glmChecks <- function(formula, data, offset, weights, datasources){
  
  # turn the formula into a character
  formula <- paste0(Reduce(paste, deparse(formula)))
  
  # replace the symbols '~', '+' and '*' by a separator
  formula <- gsub( " ", "", formula, fixed=TRUE)
  formula <- gsub( "~", "|", formula, fixed=TRUE)
  formula <- gsub( "+", "|", formula, fixed=TRUE)
  formula <- gsub( "*", "|", formula, fixed=TRUE)
  
  # split the input formula by "|" to obtain the names of the variables
  elts <- unlist(strsplit(formula, split="|", fixed=TRUE))
  
  # these identifiers are set to identify 'offset' and 'weights' which require additional checks
  varIdentifier <- rep("lpVar", length(elts))
  
  # if 'offset' and 'weights' were set add then to the variables to check
  if(!(is.null(offset))) { 
    elts <- append(elts, offset) 
    varIdentifier <- append(varIdentifier, "offset")
  }
  if(!(is.null(weights))) { 
    elts <- append(elts, weights) 
    varIdentifier <- append(varIdentifier, "weights")
  }
  
  # check that each variable is defined and not empty and each study. Stop the process if any check fails
  stdnames <- names(datasources)
  for(i in 1:length(elts)){
    if(is.na(as.numeric(elts[i], options(warn=-1)))){ # making sure an eventual intercept term is not included in the checks
      message(paste0("    ", elts[i], "..."))
      for(j in 1: length(datasources)){
        # check if the variable is defined on the server site
        myterms <- unlist(strsplit(elts[i], split='$', fixed=TRUE))
        if(length(myterms) > 1){
          cally <- call("exists", myterms[1])
          out <- opal::datashield.aggregate(datasources[j], cally)
          if(!(out[[1]])){ 
            stop(paste0("'", myterms[1], "' is not defined in ", stdnames[j], "!"), call.=FALSE)
          }else{
            cally <- paste0("colnames(", myterms[1], ")")
            clnames <- unlist(opal::datashield.aggregate(datasources[j], as.symbol(cally)))
            if(!(myterms[2] %in% clnames)){
              stop(paste0("'", myterms[2], "' is not defined in ", stdnames[j], "!"), call.=FALSE)
            }else{
              call0 <- paste0("isNaDS(", elts[i], ")")
              if(varIdentifier[i] == "offset" | varIdentifier[i] == "weights"){ typ <- checkClass(datasources, elts[i]) }
              if(varIdentifier[i] == "weights"){ call1 <- paste0("checkNegValueDS(", elts[i], ")") }              
            }
          }         
        }else{
          if(!(is.null(data))){
            cally <- paste0("colnames(", data, ")")
            clnames <- unlist(opal::datashield.aggregate(datasources[j], as.symbol(cally)))
            if(!(elts[i] %in% clnames)){
              dd <- isDefined(datasources, elts[i])
              call0 <- paste0("isNaDS(", elts[i], ")")
              if(varIdentifier[i] == "offset" | varIdentifier[i] == "weights"){ typ <- checkClass(datasources, elts[i]) }
              if(varIdentifier[i] == "weights"){ call1 <- paste0("checkNegValueDS(", elts[i], ")") }  
            }else{
              call0 <- paste0("isNaDS(", paste0(data, "$", elts[i]), ")")
              if(varIdentifier[i] == "offset" | varIdentifier[i] == "weights"){ typ <- checkClass(datasources, paste0(data, "$", elts[i])) }
              if(varIdentifier[i] == "weights"){ call1 <- paste0("checkNegValueDS(", paste0(data, "$", elts[i]), ")") }  
            }
          }else{
            defined <- isDefined(datasources, elts[i]) 
            call0 <- paste0("isNaDS(", elts[i], ")")
            if(varIdentifier[i] == "offset" | varIdentifier[i] == "weights"){ typ <- checkClass(datasources, elts[i]) }
            if(varIdentifier[i] == "weights"){ call1 <- paste0("checkNegValueDS(", elts[i], ")") } 
          }
        }
        # check if variable is not missing at complete
        out1 <- opal::datashield.aggregate(datasources[j], as.symbol(call0))
        if(out1[[1]]){ 
          stop("The variable ", elts[i], " in ", stdnames[j], " is missing at complete (all values are 'NA').", call.=FALSE)
        }
        # if offset and or weights are set check they are numeric and for weights that it does not hold negative value
        if(varIdentifier[i] == "offset" | varIdentifier[i] == "weights"){
          if(typ != 'numeric'){
            stop(paste0("'", elts[i], "' ", "is not a numeric in ", stdnames[j]), call.=FALSE)
          } 
        }
        if(varIdentifier[i] == "weights"){
          checkres <- unlist(opal::datashield.aggregate(datasources[j], as.symbol(call1)))
          if(checkres){
            stop(paste0("Negative weights not allowed - check study ", stdnames[j], "!"), call.=FALSE)
          }  
        }
        
      }
    }
  }
  
}