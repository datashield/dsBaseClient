#' 
#' @title Splits character by '$' and returns the single characters
#' @description This is an internal function.
#' @details Not required
#' @param input a vector or a list of characters
#' @keywords internal
#' @return a vector of characters
#'
extract <- function(input){
  input <- unlist(input)
  output1 <- c()
  output2 <- c()
  for (i in 1:length(input)){
    inputterms <- unlist(strsplit(input[i], "\\$", perl=TRUE))
    if(length(inputterms) > 1){
      obj1 <- strsplit(input[i], "\\$", perl=TRUE)[[1]][1]
      obj2 <- strsplit(input[i], "\\$", perl=TRUE)[[1]][2]
    }else{
      obj1 <- NA
      obj2 <- strsplit(input[i], "\\$", perl=TRUE)[[1]][1]
    }
    output1 <- append(output1, obj1)
    output2 <- append(output2, obj2)
  }
  output <- list('holders'=output1, 'elements'=output2)
  return(output)
}