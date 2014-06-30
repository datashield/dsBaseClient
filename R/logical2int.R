#' 
#' @title turns a logical operator into an integer
#' @description This is an internal function.
#' @details This function is called to turn a logical oprator given as a 
#' character into an integer: '>' is turned into 1, '>=' into 2, '<' into 3, 
#' '<=' into 4, '==' into 5 and '!=' into 6.
#' @param obj a character, the logical parameter to turn into an integer
#' @keywords internal
#' @return an integer
#'
logical2int <- function(obj=NULL){
  if(obj == ">"){ objout <- 1}
  if(obj == ">="){ objout <- 2}
  if(obj == "<"){ objout <- 3}
  if(obj == "<="){ objout <- 4}
  if(obj == "=="){ objout <- 5}
  if(obj == "!="){ objout <- 6}
  return (objout)
}