#' 
#' @title Computes the correlation between two or more vectors
#' @description This is similar to the R base function 'cor'.
#' @details In addition to computing correlations this function, unlike
#' the R base function 'cor', produces a table outlining the number of complete cases 
#' to allow for the user to make a decision about the 'relevance' of the correlation
#' based on the number of complete cases included in the correlation calculations.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @param x a character, the name of a numerical vector, matrix or dataframe
#' @param y NULL (default) or the name of a vector, matrix or data frame with compatible 
#' dimensions to x.
#' @param naAction a character string giving a method for computing covariances in the 
#' presence of missing values. This must be one of the strings: "everything", "all.obs", 
#' "complete.obs", "na.or.complete", or "pairwise.complete.obs".
#' The default value is set to "pairwise.complete.obs"
#' @return a list containing the results of the test
#' @author Gaye, A.
#' @export
#' @examples {
#' 
#' # load that contains the login details
#' data(logindata)
#' 
#' # login and assign specific variable(s)
#' # (by default the assigned dataset is a dataframe named 'D')
#' myvar <- list('LAB_HDL', 'LAB_TSC', 'GENDER')
#' opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#' # Example 1: generate the correlation matrix for the assigned dataset 'D' 
#' # which contains 4 vectors (2 continuous and 1 categorical)
#' ds.cor(x='D')
#' 
#' # Example 2: calculate the correlation between two vectors (first assign some vectors from the dataframe 'D')
#' ds.assign(newobj='labhdl', toAssign='D$LAB_HDL')
#' ds.assign(newobj='labtsc', toAssign='D$LAB_TSC')
#' ds.assign(newobj='gender', toAssign='D$GENDER')
#' ds.cor(x='labhdl', y='labtsc')
#' ds.cor(x='labhdl', y='gender')
#' 
#' }
#' 
ds.cor = function(x=NULL, y=NULL, naAction='pairwise.complete.obs', datasources=NULL)
{
  
  # if no opal login details were provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    findLogin <- getOpals()
    if(findLogin$flag == 1){
      datasources <- findLogin$opals
    }else{
      if(findLogin$flag == 0){
        stop(" Are yout logged in to any server? Please provide a valid opal login object! ", call.=FALSE)
      }else{
        message(paste0("More than one list of opal login object were found: '", paste(findLogin$opals,collapse="', '"), "'!"))
        userInput <- readline("Please enter the name of the login object you want to use: ")
        datasources <- eval(parse(text=userInput))
        if(class(datasources[[1]]) != 'opal'){
          stop("End of process: you failed to enter a valid login object", call.=FALSE)
        }
      }
    }
  }
    
  if(is.null(x)){
    stop("x=NULL. Please provide the names of two numeric vectors!", call.=FALSE)
  }
  if(is.null(y)){
    stop("y=NULL. Please provide the names of two numeric vectors!", call.=FALSE)
  }
  
  # check the type of the input object
  typ <- checkClass(datasources, x)
  
  if(typ=='numeric' | typ=='integer' | typ=='factor'){
    if(is.null(y)){
      message(" ALERT!")
      message(" y is set to NULL whilst x is a numeric vector, please provide a second vector")
      stop(" End of process!", call.=FALSE)
    }
  }
  
  if(typ=='matrix' | typ=='data.frame' & !(is.null(y))){
    y <- NULL
    warning(" x is a matrix or a dataframe; y will be ignored and a correlation matrix computed for x!")
  }
  
  # name of the studies to be used in the output
  stdnames <- names(datasources)
  
  # number of studies
  num.sources <- length(datasources)
  
  # call the server side function to compute the correlation matrix
  if(typ=='matrix' | typ=='data.frame'){
    cally <- paste0("corDS(x=", x, ", y=NULL", ", use='", naAction, "')")
  }else{
    if(!(is.null(y))){
      cally <- paste0("corDS(x=", x, ", y=", y, ", use='", naAction, "')")
    }else{
      cally <- paste0("corDS(x=", x, ", y=NULL", ", use='", naAction, "')")
    }
  }
  results <- datashield.aggregate(datasources, as.symbol(cally))
  
  message("The output is being finalized...")
  finalOutput <- vector('list', 2*num.sources)
  listnames <- c()

  if(typ=='matrix' | typ=='data.frame'){
    for(l in 1:num.sources){
      if(l > 1){ idx <- l+1}else{ idx <- l}
      finalOutput[[idx]] <- results[[l]]
      listnames <- append(listnames, paste0(stdnames[l], " --correlation"))
      completeCount <- results[[l]]
      cols <- colnames(completeCount)
      for(i in 1:dim(completeCount)[2]){
        for(j in 1:dim(completeCount)[2]){
          if(i == j){
            aa <- paste0(x,"$",cols[j])
            datashield.assign(datasources[l], 'tempvect', as.symbol(aa))
            cally <- call('subsetDS', dt='tempvect', complt=TRUE)
            datashield.assign(datasources[l], 'vect', cally)
            count <- ds.length(datasources[l], 'vect')
          }else{
            cally <- call('subsetDS', dt=x, complt=TRUE, rows=NULL, cols=c(i,j))
            datashield.assign(datasources[l], 'dt', cally)
            count <- ds.dim(datasources[l], 'dt')
          }
          completeCount[i,j] <- count[[1]][1]
        }
      }
      finalOutput[[idx+1]] <- completeCount
      listnames <- append(listnames, paste0(stdnames[l], " --number of complete cases used"))
    }

  }else{
    for(l in 1:num.sources){
      if(l > 1){ idx <- l+1}else{ idx <- l}
      finalOutput[[idx]] <- results[[l]]
      listnames <- append(listnames, paste0(stdnames[l], " --correlation"))
      
      lst <- list(x,y)
      cally <- call('cbindDS', objs=lst)
      datashield.assign(opals, 'dt', cally)
      count <- ds.dim(datasources[l], 'dt')
      
      finalOutput[[idx+1]] <- count[[1]][1]
      listnames <- append(listnames, paste0(stdnames[l], " --number of complete cases used"))
    }
  }
  
  names(finalOutput) <- listnames
  return(finalOutput)
  
}
