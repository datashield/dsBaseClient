#'
#' @title Produces the pooled beta values and standard errors
#' @description This is an internal function required by the client
#' function \code{ds.gee}.
#' @param score a numeric vector, a score vector
#' @param infoMatrix a matrix, the information matrix
#' @param Jmatrix a matrix, the J matrix
#' @param startCoeff a numeric vector, the starting values for the beta coefficients.
#' @keywords internal
#' @return a list which contains the individual elements of the input expression
#' @author Gaye, A.; Jones EM.
#' 
geehelper2 <- function(score, infoMatrix, Jmatrix, startCoeff){
  if(is.null(startCoeff)) {
    startCoeff <- rep(0, length(score[[1]]))
  }
  
  sum.scores <- as.list(rep(0, length(startCoeff)))
  rows <- dim(infoMatrix[[1]])[1]
  cols <- dim(infoMatrix[[1]])[2]
  sum.I.matrices <- matrix(0, nrow=rows, ncol=cols)
  
  # loop through studies and sum up matrices and vectors separately
  for(i in 1:length(startCoeff)){
    for(s in 1:length(score)){
      sum.scores[[i]] <- sum.scores[[i]] + score[[s]][i]
    }
  }
  
  for(i in 1: length(infoMatrix)){
    sum.I.matrices <- sum.I.matrices + infoMatrix[[i]]
  }
  
  # add up start values for beta, score vector and info matrix
  beta.vector <- startCoeff + (solve(sum.I.matrices)%*%unlist(sum.scores))
  
  # compute the 'empirical' standard errors of the beta values (get the Jmatrix first)
  rows <- dim(Jmatrix[[1]])[1]
  cols <- dim(Jmatrix[[1]])[2]
  sum.J.matrices <- matrix(0, nrow=rows, ncol=cols)
  for(i in 1: length(Jmatrix)){
    sum.J.matrices <- sum.J.matrices + Jmatrix[[i]]
  }
  v.beta.hat <- solve(sum.I.matrices)%*%sum.J.matrices%*%solve(sum.I.matrices)
  stderrs <- sqrt(diag(v.beta.hat))
  
  # output
  return(list("beta.values"=beta.vector, "standard.errors"=stderrs))
}