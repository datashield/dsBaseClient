#' 
#' @title Calculates the covariance between two variables
#' @description This function calculates the covariance of two variables or the variance-covariance 
#' matrix for the vairables of an input dataframe
#' @details In addition to computing covariances; this function, produces a table outlining the 
#' number of complete cases and a table outlining the number of missing values to allow for the
#' user to make a decision about the 'relevance' of the covariance based on the number of complete
#' cases included in the covariance calculations.
#' @param x a character, the name of a vector, matrix or dataframe of variable(s) for which the
#' covariance(s) is (are) calculated for.
#' @param y NULL (default) or the name of a vector, matrix or dataframe with compatible 
#' dimensions to x.
#' @param naAction a character string giving a method for computing covariances in the 
#' presence of missing values. This must be one of the strings "casewise.complete" or 
#' "pairwise.complete". If \code{use} is set to 'casewise.complete', then the function omits all the rows
#' in the whole dataframe that include at least one cell with a missing value before the calculation of covariances.
#' If \code{use} is set to 'pairwise.complete' (default), then the function divides the input dataframe to subset
#' subset dataframes formed by each pair between two variables (all combinations are considered) and omits the rows
#' with missing values at each pair separately and then calculates the covariances of those pairs.
#' @param type a character which represents the type of analysis to carry out. If \code{type} is
#' set to 'split' (default), the covariance of two variables or the variance-covariance matrix of
#' an input dataframe and the number of complete cases and missing values are returned for each
#' single study. If \code{type} is set to 'combine', the pooled covariance, the total number of
#' complete cases and the total number of missing values aggregated from all the involved studies,
#' are returned. 
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @return a list containing the number of missing values in each variable, the number of missing variables 
#' casewise or paiwise depending on the argument \code{use}, the covariance matrix, the number of used complete cases
#' and an error message which indicates whether or not the input variables pass the disclosure control (i.e. none of them
#' is dichotomous with a level having less counts than the pre-specified threshold). If any of the input variables does not
#' pass the disclosure control then all the output values are replaced with NAs. If all the variables are valid and pass 
#' the control, then the output matrices are returned and also an error message is returned but it is replaced by NA.
#' @author Gaye A; Avraam D; Burton PR
#' @export
#' @examples
#' \dontrun{
#'
#' #  # load that contains the login details
#' #  data(glmLoginData)
#' #  library(opal)
#' #
#' #  # login and assign specific variable(s)
#' #  # (by default the assigned dataset is a dataframe named 'D')
#' #  myvar <- list('LAB_HDL', 'LAB_TSC', 'LAB_GLUC_ADJUSTED', 'GENDER')
#' #  opals <- opal::datashield.login(logins=glmLoginData, assign=TRUE, variables=myvar)
#' #
#' #  # Example 1: generate the covariance matrix for the assigned dataset 'D' 
#' #  # which contains 4 vectors (3 continuous and 1 categorical)
#' #  ds.cov.o(x='D')
#' #
#' #  # Example 2: generate the covariance matrix for the dataset 'D' combined for all 
#' #  # studies and removing any missing values casewise 
#' #  ds.cov.o(x='D', naAction='casewise.complete', type='combine')
#' #
#' #  # Example 3: calculate the covariance between two vectors 
#' #  # (first assign the vectors from 'D')
#' #  ds.assign(newobj='labhdl', toAssign='D$LAB_HDL')
#' #  ds.assign(newobj='labtsc', toAssign='D$LAB_TSC')
#' #  ds.assign(newobj='gender', toAssign='D$GENDER')
#' #  ds.cov.o(x='labhdl', y='labtsc', naAction='pairwise.complete', type='combine')
#' #  ds.cov.o(x='labhdl', y='labtsc', naAction='casewise.complete', type='combine')
#' #  ds.cov.o(x='labhdl', y='gender', naAction='pairwise.complete', type='combine')
#' #  ds.cov.o(x='labhdl', y='gender', naAction='casewise.complete', type='combine')
#' #
#' #  # clear the Datashield R sessions and logout
#' #  opal::datashield.logout(opals)
#' 
#' }
#'
ds.cov.o <- function(x=NULL, y=NULL, naAction='pairwise.complete', type="split", datasources=NULL){
  
  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }
  
  if(is.null(x)){
    stop("x=NULL. Please provide the name of a matrix or dataframe or the names of two numeric vectors!", call.=FALSE)
  }else{
    defined <- isDefined(datasources, x)
  }
  
  # check the type of the input objects
  typ <- checkClass(datasources, x)
  
  if(typ=='numeric' | typ=='integer' | typ=='factor'){
    if(is.null(y)){
      stop("If x is a numeric vector, y must be a numeric vector!", call.=FALSE)
    }else{
      defined2 <- isDefined(datasources, y)
      typ2 <- checkClass(datasources, y)
    }
  }
  
  if(typ=='matrix' | typ=='data.frame' & !(is.null(y))){
    y <- NULL
    warning("x is a matrix or a dataframe; y will be ignored and a covariance matrix computed for x!")
  }

  # name of the studies to be used in the output
  stdnames <- names(datasources)

  # call the server side function
  if(typ=='matrix' | typ=='data.frame'){
    cally <- paste0("covDS.o(x=", x, ", y=NULL", ", use='", naAction, "')")
  }else{
    if(!(is.null(y))){
      cally <- paste0("covDS.o(x=", x, ", y=", y, ", use='", naAction, "')")
    }else{
      cally <- paste0("covDS.o(x=", x, ", y=NULL", ", use='", naAction, "')")
    }
  }
  output <- opal::datashield.aggregate(datasources, as.symbol(cally))

  if (type=="split"){
    covariance <- list()
    results <- list() 
    for(i in 1:length(stdnames)){
      covariance[[i]] <- matrix(0, ncol=dim(output[[i]][[1]])[2], nrow=dim(output[[i]][[1]])[1])
      colnames(covariance[[i]]) <- colnames(output[[i]][[1]])
      rownames(covariance[[i]]) <- colnames(output[[i]][[1]])
      for(m in 1:dim(output[[i]][[1]])[1]){
        for(n in 1:dim(output[[i]][[1]])[2]){
          if (naAction=='pairwise.complete'){
            covariance[[i]][m,n] <- (1/(output[[i]][[3]][m,n]-1))*(output[[i]][[1]][m,n])-(1/(output[[i]][[3]][m,n]*(output[[i]][[3]][m,n]-1)))*output[[i]][[2]][m,n]*output[[i]][[2]][n,m]     
          }
          if (naAction=='casewise.complete'){
            covariance[[i]][m,n] <- (1/(output[[i]][[3]][m,n]-1))*(output[[i]][[1]][m,n])-(1/(output[[i]][[3]][m,n]*(output[[i]][[3]][m,n]-1)))*output[[i]][[2]][m]*output[[i]][[2]][n]     
          }                  
       }
      }
      results[[i]] <- list(output[[i]][[4]][[1]], output[[i]][[4]][[2]], covariance[[i]], output[[i]][[3]], output[[i]][[5]])
      n1 <- "Number of missing values in each variable"
      if(naAction=='casewise.complete'){
        n2 <- "Number of missing values casewise"
      }
      if(naAction=='pairwise.complete'){
        n2 <- "Number of missing values pairwise"
      }
      n3 <- "Variance-Covariance Matrix"
	  n4 <- "Number of complete cases used"
      n5 <- "Error message"
      names(results[[i]]) <- c(n1, n2, n3, n4, n5)
    }
  }else{
    if (type=="combine"){
      combined.sums.of.products <- matrix(0, ncol=dim(output[[1]][[1]])[2], nrow=dim(output[[1]][[1]])[1])
      combined.sums <- matrix(0, ncol=dim(output[[1]][[2]])[2], nrow=dim(output[[1]][[2]])[1])
      combined.complete.cases <- matrix(0, ncol=dim(output[[1]][[3]])[2], nrow=dim(output[[1]][[3]])[1])
      combined.missing.cases.vector <- matrix(0, ncol=dim(output[[1]][[4]][[1]])[2], nrow=dim(output[[1]][[4]][[1]])[1])
      combined.missing.cases.matrix <- matrix(0, ncol=dim(output[[1]][[4]][[2]])[2], nrow=dim(output[[1]][[4]][[2]])[1])
      combined.error.message <- list()
      for(i in 1:length(stdnames)){
        combined.sums.of.products <- combined.sums.of.products + output[[i]][[1]]
	  combined.sums <- combined.sums + output[[i]][[2]]
        combined.complete.cases <- combined.complete.cases + output[[i]][[3]]
        combined.missing.cases.vector <- combined.missing.cases.vector + output[[i]][[4]][[1]]
        combined.missing.cases.matrix <- combined.missing.cases.matrix + output[[i]][[4]][[2]]
	  combined.error.message[[i]] <- output[[i]][[5]]
      }

	combined.covariance <- matrix(0, ncol=dim(output[[1]][[1]])[2], nrow=dim(output[[1]][[1]])[1])
	colnames(combined.covariance) <- colnames(output[[1]][[1]])
      rownames(combined.covariance) <- colnames(output[[1]][[1]])
      for(m in 1:dim(output[[i]][[1]])[1]){
        for(n in 1:dim(output[[i]][[1]])[1]){
          if (naAction=='pairwise.complete'){
            combined.covariance[m,n] <- (1/(combined.complete.cases[m,n]-1))*(combined.sums.of.products[m,n])-(1/(combined.complete.cases[m,n]*(combined.complete.cases[m,n]-1)))*combined.sums[m,n]*combined.sums[n,m]     
          }
          if (naAction=='casewise.complete'){
            combined.covariance[m,n] <- (1/(combined.complete.cases[m,n]-1))*(combined.sums.of.products[m,n])-(1/(combined.complete.cases[m,n]*(combined.complete.cases[m,n]-1)))*combined.sums[m]*combined.sums[n]     
          }
        }
      }
	  
      results <- list(combined.missing.cases.vector, combined.missing.cases.matrix, combined.covariance, combined.complete.cases, combined.error.message)
      n1 <- "Number of missing values in each variable"
      if(naAction=='casewise.complete'){
        n2 <- "Number of missing values casewise"
      }
      if(naAction=='pairwise.complete'){
        n2 <- "Number of missing values pairwise"
      }
      n3 <- "Variance-Covariance Matrix"
	  n4 <- "Number of complete cases used"
      n5 <- "Error message"
      names(results) <- c(n1, n2, n3, n4, n5)
    }else{
      stop('Function argument "type" has to be either "combine" or "split"')
    }
  }

  return(results)  

}
