#'
#' @title Checks validity of tabulated factor variable
#' @description this is an INTERNAL function required by the client function \code{ds.table1D}. 
#' @details It verifies the cell counts of the tabulate vectors which are
#' considered valid if no count > 0 and < 5 and not valid otherwise.
#' @param input a list object returned by the server side function \code{table1dDS}
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
table1dhelper1 <- function (input){
  
  # first script required by the function ds.table1d
  
  # number of studies
  num.opals <- length(input)
  
  # Count valid tables
  num.valid.tables <- 0
  opals.valid.id <- NULL
  opals.valid.binary <- rep(NA,num.opals)

  for(j in 1:num.opals)
  {
    opals.valid.binary[j] <- as.integer(input[[j]]$is.factor.valid)
    if(as.integer(input[[j]]$is.factor.valid)==1)opals.valid.id <- c(opals.valid.id,j)
    num.valid.tables <- num.valid.tables+as.integer(input[[j]]$is.factor.valid) 
  }

  # check at least one study is valid
  zero.studies.valid <- FALSE
  if(num.valid.tables < 1 )zero.studies.valid <- TRUE

  # get names of valid studies - get the character ids corresponding to the obtained indices
  #opals.valid.id <- names(input)[opals.valid.id]
  

  obj.2.return <- list(num.opals, num.valid.tables, opals.valid.binary, opals.valid.id, zero.studies.valid)
  names(obj.2.return) <-  c("num.opals", "num.valid.tables", "opals.valid.binary", "opals.valid.id", "zero.studies.valid")
  return (obj.2.return)
} 
