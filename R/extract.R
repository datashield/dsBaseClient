#' 
#' @title Splits character by '$' and returns the single characters
#' @description This is an internal function.
#' @details Not required
#' @param input a character with'$' signs within it
#' @keywords internal
#' @return a vector of characters
#'
extract <- function(input){
  inputterms <- unlist(strsplit(input, "\\$", perl=TRUE))
  if(length(inputterms) > 1){
    objnames <- strsplit(input, "\\$", perl=TRUE)[[1]]
  }else{
    objnames <- input
  }
  return(objnames)
}