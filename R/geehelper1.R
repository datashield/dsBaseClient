#'
#' @title Computes the pooled parameters alpha and phi
#' @description This is an internal function required by the client
#' function \code{ds.gee}.
#' @param N a numeric vector, the sample sizes of the studies
#' @param npara the number of parameters/columns of the design matrix, for a regression
#' model, generated from the table of individual level data.
#' @param M_study 
#' @param alphaM
#' @param sum_p
#' @param corStructure an integer that set the correlation structure: 1 for 'ar1',
#' 2 for 'exchangeable', 3 for 'independence', 4 for for 'fixed' and 5 for an 
#' 'unstructure' correlation structure.
#' @keywords internal
#' @return a list which contains the individual elements of the input expression
#' @author Gaye, A.; Jones EM.
#' 
geehelper1 <- function(N, npara, M_study, alphaM, sum_p, corStructure=NULL){
  N <- sum(N)
  npara <- npara[1]
  phi<-(N-npara)^(-1)*sum(sum_p)
  
  if(corStructure=="unstructured"){
    M <- phi*((-999)-npara) 
  }else{
    M <- phi*(sum(M_study)-npara) 
  }
  
  if(length(alphaM) > 1){
    dims <- c()
    for(i in 1:length(alphaM)){
      dims <- append(dims, length(alphaM[[i]]))
    }
    sum.alpha <- rep(0, max(dims))
    for(i in 1:max(dims)){
      for(s in 1:length(alphaM)){
        sum.alpha[i] <- sum(sum.alpha[i],alphaM[[s]][i], na.rm=T)
      }
    }
  }else{
    sum.alpha <- unlist(alphaM)
  }
  alpha <- M^(-1)*sum.alpha  
  
  output <- list(alpha=alpha, phi=phi)
  
  return(output)
}
