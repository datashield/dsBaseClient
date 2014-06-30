#'
#' @title Checks validity of 2D-tables
#' @description this is an INTERNAL function.
#' @details The function is required required by the client function \code{ds.table2D}. 
#' @param input a list object returned by the server side function \code{table2dDS}
#' @param varname1 a character string, the name of the first input numerical vector
#' @param varname2 a character string, the name of the second input numerical vector
#' @keywords internal
#' @return a list that contains the following elements:
#' \code{num.opals} the number of studies to analyse 
#' \code{num.valid.tables} number of studies with valid tabulated vectors (no cell with count > 0 and < 5)
#' \code{opals.valid.binary} vector of binaries indicating whether or not the tabulate vector
#' was valid or not (0 = not valid and 1 = not valid)
#' \code{opals.valid.id} a vector that gives the indices of the studies with valid tabulated vectors
#' \code{zero.studies.valid} a boolean indicating if all studies were not valid,
#' 'TRUE' if none of the studies is valid, 'FALSE' otherwise
#' @author Burton, P.; Gaye, A.
#' 
table2dhelper1 <- function(input, varname1, varname2){
  
  # count total opals
  num.sources <- length(input)

  # use specified variable names from call script to replace names allocated by server side function
  for(r in 1:num.sources)
  {
    names(dimnames(input[[r]]$safe.table))[1] <- as.character(varname1)
    names(dimnames(input[[r]]$safe.table))[2] <- as.character(varname2)
  }

  # count valid tables
  num.valid.tables <- 0
  opals.valid.id <- NULL
  opals.valid.binary <- rep(NA,num.sources)

  for(j in 1:num.sources)
  {
    opals.valid.binary[j] <- as.integer(input[[j]]$is.table.valid)
    if(as.integer(input[[j]]$is.table.valid)==1)opals.valid.id <- c(opals.valid.id,j)
    num.valid.tables <- num.valid.tables+as.integer(input[[j]]$is.table.valid) 
  }

  zero.studies.valid <- FALSE
  if(num.valid.tables < 1) {zero.studies.valid <- TRUE}

  # return 5 key output objects:
  obj2return <- list(num.sources, num.valid.tables, opals.valid.binary, opals.valid.id, zero.studies.valid)
  names(obj2return) <- c("num.sources", "num.valid.tables", "opals.valid.binary", "opals.valid.id", "zero.studies.valid")
  return(obj2return)
}



