#' 
#' @title Logs in and assigns variables to R
#' @description This function allows for clients to login to opal servers 
#' and (optionaly) assign all the data or specific variables from Opal 
#' datasources to R. The assigned dataframes (one for each opal server) 
#' are named 'D'.
#' @param logins a dataframe table that holds login details. This table holds five elements 
#' required to login to the servers where the data to analyse is stored. 
#' See the documentation of the examplar input table \code{logindata} for details of the login 
#' elements.
#' @param assign a boolean which tells whether or not data should be assigned from the opal 
#' datasource to R after login into the server(s).
#' @param variables specific variables to assign. If \code{assign} is set to FALSE
#' this argument is ignored otherwise the specified variables are assigned to R.
#' If no variables are specified (default) the whole dataset is assigned.
#' @return object(s) of class opal
#' @author Gaye, A.
#' @export
#' @examples {
#' 
#' #### The below examples illustrate an analysises that use test/simulated data ####
#' 
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
  
  # issue an alert and stop the process if no login table is provided
  if(is.null(logins)){
    stop(" Provide valid login details!\n\n")
  }
  
  # studies names
  stdnames <- as.character(logins$server)
  
  # URLs 
  urls <- as.character(logins$url)
  
  # usernames
  userids <- as.character(logins$userID)
  
  # passwords
  pwds <- as.character(logins$pwd)
  
  # opal directories where the microdata is stored
  paths <- as.character(logins$opalPath)
  
  # put the server names in a list
  opals <- as.list(stdnames)  
  
  # login to the opals keeping the server names as specified in the login file
  cat("\nLogging into the collaborating servers\n")
  opals <- vector("list", length(urls))
  names(opals) <- as.character(logins[,1])
  for(i in 1:length(opals)) {
    # if the connection is HTTPS use ssl options else they are not required
    protocol <- strsplit(urls[i], split="://")[[1]][1]
    if(protocol=="https"){
      credentials <- list(sslcert=userids[i],sslkey=pwds[i],ssl.verifyhost=0,ssl.verifypeer=0,sslversion=3)
      opals[[i]] <- opal.login(url=urls[i], opts=credentials)        
    }else{
      opals[[i]] <- opal.login(username=userids[i], password=pwds[i], url=urls[i])  
    }
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
      cat("\nAssigining data:\n")
      for(i in 1:length(opals)) {
        cat(stdnames[i],"\n")
        datashield.assign(opals[[i]], "D", paths[i])
      }
      cat("\nVariables assigned:\n")
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
  
  # return the 'opal' object
  return(opals)
  
}
