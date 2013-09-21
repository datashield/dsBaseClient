#' 
#' @title Logs in and assigns variables to R
#' @description This function allows for clients to login to opal servers 
#' and (optionaly) assign all the data or specific variables from Opal 
#' datasources to R. The assigned dataframes (one for each opal server) 
#' are named 'D'.
#' @param logins a dataframe that holds login details
#' @param assign a boolean which tells whether or not data should 
#' from the opal datasource to R after login into the server(s).
#' @param variables specific variables to assign. If \code{assign} is set to FALSE
#' this argument is ignored otherwise the specified variables are assign to R.
#' If no variables are specified (default) the whole dataset is assigned.
#' @return an object of class opal
#' @author Gaye, A.
#' @export
#' @examples {
#' # load that contains the login details
#' data(logindata)
#' 
#' # Example 1: just login (default)
#' opals <- ds.login(logins=logindata)
#'
#' # Example 2: login and assign the whole dataset
#' opals <- ds.login(logins=logindata,assign=TRUE)
#' 
#' # Example 3: login and assign specific variable(s)
#' myvar <- list("LAB_TSC")
#' opals <- ds.login(logins=logindata,assign=TRUE,variables=myvar)
#' }
#' 
ds.login <- function(logins=NULL, assign=FALSE, variables=NULL){
  
  if(is.null(logins)){
    stop(" Provide valid login details!\n\n")
  }

  # URLs 
  urls <- as.character(logins[,2])
  
  # usernames
  userids <- as.character(logins[,3])
  
  # passwords
  pwds <- as.character(logins[,4])
  
  # opal directories where the microdata is stored
  paths <- as.character(logins[,5])
  
  # studies names
  stdnames <- as.character(logins[,1])
  
  # put the server names in a list
  opals <- as.list(as.character(logins$servers))
  
  # login to the opals keeping the server names as 
  # specified in the login file
  cat("\nLogging into the collaborating servers\n")
  opals <- vector("list", length(urls))
  names(opals) <- as.character(logins[,1])
  for(i in 1:length(opals)) {
     opals[[i]] <- opal.login(userids[i], pwds[i], urls[i],
                   opts=list(ssl.verifyhost=0,ssl.verifypeer=0))
  }
  
  # if argument 'assign' is true assign data to the opal server(s) you logged 
  # in to. If no variables are specified the whole dataset is assigned
  # i.e. all the variables in the opal database are assigned
  if(assign){
    if(is.null(variables)){
      # if the user does not specify variables (default behaviour)
      # display a message telling the user that the whole dataset
      # will be assigned since he did not specify variables
      cat("\n  No variables have been specified. \n  All the variables in the opal datasource \n  (the whole dataset) will be assigned to R!\n\n")
      cat("Assigining data\n")
      for(i in 1:length(opals)) {
        datashield.assign(opals[[i]], "D", paths[i])
      }
      cat("Variables assigned:\n")
      varnames <- datashield.aggregate(opals[1], quote(colnames(D)))
      cat(paste(unlist(varnames), collapse=", "), "\n\n")
    }else{
      cat("\nAssigining data:\n")
      for(i in 1:length(opals)) {
        cat(stdnames[i],"\n")
        datashield.assign(opals[[i]], "D", paths[i], variables)
      }
      cat("\nVariables assigned:\n")
      cat(paste(unlist(variables), collapse=", "), "\n\n")
    }
  }
  # return the 'opals' object
  return(opals)
}
