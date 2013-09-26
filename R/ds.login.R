#' 
#' @title Logs in and assigns variables to R
#' @description This function allows for clients to login to opal servers 
#' and (optionaly) assign all the data or specific variables from Opal 
#' datasources to R. The assigned dataframes (one for each opal server) 
#' are named 'D'. The function can be uses to run analysis using test/simulated data
#' or real data sitting behind the firewall of a study/cohort. Specific credentials
#' including valid datashield certificate and key are required to run a real data analysis.
#' To obtain the necessary credentials the user must contact the studies/cohort whose data 
#' he want to use and the DataSHIELD development team.
#' @param realdata a boolean that indicates if the user is carrying out a datashield analysis
#' using real data held behind the firewalls of collaborating studies/cohorts. The default value
#' is FALSE i.e. the user is not using real data rather he/she is using some test data (e.g. simulated data).
#' @param logins a dataframe that holds login details
#' @param assign a boolean which tells whether or not data should 
#' from the opal datasource to R after login into the server(s).
#' @param variables specific variables to assign. If \code{assign} is set to FALSE
#' this argument is ignored otherwise the specified variables are assign to R.
#' If no variables are specified (default) the whole dataset is assigned.
#' @param certificate a character string, the path to a valid datashield ssl certificate, 
#' required to run an analysis using real data (as opposed to test/simulated data). 
#' The certificate is required to log into to the server that holds the real data behind the 
#' firewall of a collaborating study. 
#' @param key a character string, the path to a valid datashield ssl key, 
#' required to run an analysis using real data (as opposed to test/simulated data). 
#' The key is required to log into to the server that holds the real data behind the 
#' firewall of a collaborating study. 
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
ds.login <- function(realdata=FALSE, logins=NULL, assign=FALSE, variables=NULL, certificate=NULL, key=NULL){
  
  # issue an alert and stop the process if no login table is provided
  if(is.null(logins)){
    stop(" Provide valid login details!\n\n")
  }
  
  # studies names
  stdnames <- logins$server
  
  # URLs 
  urls <- logins$url
  
  # usernames
  userids <- logins$userID
  
  # passwords
  pwds <- logins$pwd
  
  # opal directories where the microdata is stored
  paths <- as.character(logins[,5])
  
  # put the server names in a list
  opals <- as.list(stdnames)  
  
  
  # if 'realdata' is set to TRUE issue a message telling what is required
  if(realdata){
    
    # issue a message when the user want to run an analysis using real data
    cat("\nA valid certificate and a private key are required for an analysis using real data!\n")
    cat("The variables 'userIDs' and 'pwds' in the 'logins' table are not required and will be ignored!\n")
    
    # issue an alert and stop the process if no certificate or key are provided
    if(is.null(certificate)){
      stop(" Provide a valid path to the datashield certificate\n\n")
    }
    
    if(is.null(key)){
      stop(" Provide a valid path to the private key\n\n")
    } 
    
    # login to the opals keeping the server names as specified in the login file
    cat("\nLogging into the collaborating servers\n")
    opals <- vector("list", length(urls))
    names(opals) <- as.character(logins[,1])
    credentials <- list(sslcert=certificate,sslkey=key,ssl.verifyhost=0,ssl.verifypeer=0,sslversion=3)
    for(i in 1:length(opals)) {
      opals[[i]] <- opal.login(url=urls[i], opts=credentials)
    }
    
  }else{
    # issue a message when the user want to run an analysis using test data
    cat("\nA valid username and a valid password are required for an analysis using test data!\n")
    cat("The variable 'userids' in the 'logins' table must be a valid username\n")
    cat("The variable 'pwds' in the 'logins' table must be a valid password\n\n")
    
    # login to the opals keeping the server names as specified in the login file
    cat("\nLogging into the collaborating servers\n")
    opals <- vector("list", length(urls))
    names(opals) <- as.character(logins[,1])
    for(i in 1:length(opals)) {
      credentials <- list(ssl.verifyhost=0,ssl.verifypeer=0,sslversion=0)      
      opals[[i]] <- opal.login(userids[i],pwds[i],urls[i],opts=credentials)
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
