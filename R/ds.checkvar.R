#'
#' @title Checks if all variables do exist and are not empty
#' @description This function check that the variables to analyse are (1) available from all 
#' the studies and (2) that they do not contain only missing values (NAs). It excludes studies 
#' that fail any of these two checks.
#' @details if this function is given a variable(s) created/derived from a variable(s) initially
#' assigned using through 'datashield.login' only the second check is carried out as it is not then 
#' necessary to check is the variable is available.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal 
#' datasources.
#' @param variables a character vector, the names of the variable(s) to check 
#' @return the opal objects which passed the checks
#' @author Gaye, A.
#' @export
#' @examples {
#' 
#' # load that contains the login details
#' data(logindata)
#' 
#' # login and assign specific variable(s)
#' myvar <- list("LAB_TSC")
#' opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#' # Example 1: run checks for the variable LAB_TSC assigned above
#' opals <- ds.checkvar(datasources=opals, variables=list(quote(D$LAB_TSC)))
#' 
#' # Example 2: lets assign LAB_TSC to another variable 'tsc' not attached to 'D' and check 'tsc'
#' datashield.assign(opals, 'tsc', quote(D$LAB_TSC))
#' opals <- ds.checkvar(datasources=opals, variables=list(quote(tsc)))
#' }
#'
ds.checkvar <- function(datasources=NULL, variables=NULL){
  
  if(is.null(datasources)){
    cat("\n\n ALERT!\n")
    cat(" No valid opal object(s) provided.\n")
    cat(" Make sure you are logged in to valid opal server(s).\n")
    stop(" End of process!\n\n", call.=FALSE)
  }
  
  if(is.null(variables)){
    cat("\n\n ALERT!\n")
    cat(" Please provide a list for the argument 'variables'\n")
    stop(" End of process!\n\n", call.=FALSE)
  }
  
  inputterms <- unlist(strsplit(deparse(variables[[1]]), "\\$", perl=TRUE))
  if(length(inputterms) > 1){
    # print a message for the user informing of checks
    cat("\nChecks are carried out on the variables used for the analysis\nto ensure they are available from the dataset(s) and not empty.\n\n")
    
    # get the names of the opal servers/studies
    stdname <- names(datasources)
    
    # the input variable might be given as column table (i.e. D$xvect)
    # or just as a vector not attached to a table (i.e. xvect)
    # we have to make sure the function deals with each case
    # get the names of the variables to check
    varIDs <- vector("character", length(variables))
    for(i in 1:length(variables)){
      xx <- variables[[i]]
      varIDs[i] <- strsplit(deparse(xx), "\\$", perl=TRUE)[[1]][2]
    }
    
    # a vector that keeps the results of the checks for each study
    toremove <- c()
    
    # loop through the dataset(s) and the variable(s)
    symbol <- strsplit(deparse(variables[[1]]), "\\$", perl=TRUE)[[1]][1]
    for(i in 1: length(datasources)){
      
      # Carry out the first check:  are all the variables to analyse available from dataset
      track <- FALSE
      # get the names of the variables in the assigned dataset
      var.names <- datashield.aggregate(datasources[i],  paste0('colnames(',symbol,')'))   
      
      # check if any of the variables in the arguments is missing from the assigned dataset
      idx1 <- which(!(varIDs %in% var.names[[1]]))
      missings <- length(idx1)
      if(missings > 0){
        # record that the stduy has failed the first checks and print a message
        track <- TRUE
        cat("The variable(s)", varIDs[idx1], "is/are missing from", stdname[i],"!\n") 
        
      }else{
        # carry out the second check: do any of the variables to anlyse contain only NAs
        # this second check is carried out only if the first check is negative
        # loop through the variables in the argument and if any fails break out the loop
        
        # get the indices, in the assigned dataset, of the variables to check
        idx2 <- which(var.names[[1]] %in% varIDs)
        counter <- 1
        for(j in idx2){
          # the server side function 'isNA.ds' to check if vector is empty
          cally <- call("isNA.ds", variables[[counter]])
          out <- datashield.aggregate(datasources[i], cally)
          if(out[[1]]){ 
            track <- TRUE
            cat("The variable", var.names[[1]][j], "in", stdname[i], "is empty (NAs only)!\n")
          }
          counter <- counter+1
        }
      }
      
      # if a study fails any of the two checks add it to the list of studies to exclude and print a message
      if(track){ 
        toremove <- append(toremove, i)
        cat(stdname[i], "will not be included in the analysis\n\n") 
      }
      
    }
    
    # remove studies which contain one or more variables that failed the checks
    if(length(toremove) > 0){
      datasources <- datasources[-toremove] 
    }else{
      cat("The checks went fine: all variables are available and none has all its values missing!\n\n")
    }
    
    # If none of the datasets passed the checks stop the process
    # ortherwise return the opal objects that passed the checks
    if(length(datasources) == 0){
      stop("The variables specified in the arguments are not available or contain only missing values, in all the assigned datasets!")
    }else{
      return(datasources)
    }
    
  }else{
    
    # print a message for the user informing of checks
    cat("\nChecks are carried out on the input variable(s) to ensure they are not empty (i.e. all entries are NA)\n\n")
    
    # get the names of the opal servers/studies
    stdname <- names(datasources)
    
    # get the names of the variables to check
    varIDs <- vector("character", length(variables))
    for(i in 1:length(variables)){
      xx <- variables[[i]]
      varIDs[i] <- deparse(xx)
    }
    
    # a vector that keeps the results of the checks for each study
    toremove <- c()
    track <- FALSE
    
    # loop through the dataset(s) and the variable(s)
    symbol <- strsplit(deparse(variables[[1]]), "\\$", perl=TRUE)[[1]][1]
    for(i in 1: length(datasources)){
      
      # carry out the second check: do any of the input variables contain only NAs
      # loop through the variables in the argument and if any fails break out the loop
      
      # get the indices, in the assigned dataset, of the variables to check
      var.names <- varIDs
      for(i in 1:length(datasources)){
        for(j in 1:length(var.names)){
          # call the server side function 'isNA.ds' to check if vector is empty
          cally <- call("isNA.ds", variables[[j]])
          out <- datashield.aggregate(datasources[i], cally)
          if(out[[1]]){ 
            track <- TRUE
            cat("The variable", var.names[j], "in", stdname[i], "is empty (all values are NA)!\n")
          }
        }
      }
      
      # if a study fails any of the two checks add it to the list of studies to exclude and print a message
      if(track){ 
        toremove <- append(toremove, i)
        cat(stdname[i], "will not be included in the analysis\n\n") 
      }
      
    }
    
    # remove studies which contain one or more variables that failed the checks
    if(length(toremove) > 0){
      datasources <- datasources[-toremove] 
    }else{
      cat("The checks went fine: no variable has all its values missing!\n\n")
    }
    
    # If the variable(s) did not pass the checks for any of the studies stop the process and display a message
    # ortherwise return the opal objects that passed the checks
    if(length(datasources) == 0){
      stop("\nall the observations in the input variable(s) have missing values, in all the studies!\n")
    }else{
      return(datasources)
    }
    
  }
  

}
