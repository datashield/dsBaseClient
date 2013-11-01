#' 
#' @title Generates a valid subsets of a dataset
#' @details The function takes a dataframe and generates a subset
#' dataframes for all the factor variables. By default a subset of the dataframe 
#' is created for each category of each factor. It is possible to indicate the variable 
#' for which a subset is sought by indicating their columns indices. If the variables
#' at the given indices are not factors no subsets are generated.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @param dataset a a string character, the name of the dataset.
#' @param columns a numeric list vector that gives the indices of the
#' variables for which subsets are sougth.
#' @param newobj the name of the output object. If this argument is set to NULL, 
#' the name of the new object is 'subsets'.
#' @return a message is displayed when the action is completed.
#' @author Gaye, A. (amadou.gaye@bristol.ac.uk)
#' @export
#' @examples {
#' 
#' # load the login data
#' data(logindata)
#' 
#' # login and assign some variables to R
#' myvar <- list("DIS_DIAB","PM_BMI_CONTINUOUS","LAB_HDL", "GENDER")
#' opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#' # Example 1: get all subsets from the table assigned above (by default the table name is 'D')
#' ds.subsetdata(datasources=opals, dataset="D")
#' 
#' #' # Example 2: get specific subsets from the table assigned above (by default the table name is 'D')
#' ds.subsetdata(datasources=opals, dataset="D", columns=list(4))
#' }
#' 
ds.subsetdata = function(datasources=NULL, dataset=NULL, columns=NULL, newobj=NULL){
  
  if(is.null(datasources)){
    message("\n ALERT!\n")
    message(" No valid opal object(s) provided.")
    message(" Make sure you are logged in to valid opal server(s).\n")
    stop(" End of process!\n", call.=FALSE)
  }
  
  if(is.null(dataset)){
    message("\n ALERT!\n")
    message(" Please provide a valid input dataset.")
    stop(" End of process!\n", call.=FALSE)
  }
  
  # create a name by default if user did not provide a name for the new variable
  if(is.null(newobj)){
    newobj <- "subsets"
  }
  
  # call the server side function that does the job
  cally <- call('subsetdata.ds', dataset, columns )
  datashield.assign(datasources, newobj, cally)
  
  # a message so the user know the function was ran (assign function are 'silent')
  message("An 'assign' function was ran, no output should be expected on the client side!")
  
  # check that the new object has been created and display a message accordingly
  cally <- call('exists', newobj )
  qc <- datashield.aggregate(datasources, cally)
  indx <- as.numeric(which(qc==TRUE))
  if(length(indx) == length(datasources)){
    message("The output of the function, '", newobj, "', is stored on the server side.")
  }else{
    if(length(indx) > 0){
      warning("The output object, '", newobj, "', was generated only for ", names(datasources)[indx], "!")
    }
    if(length(indx) == 0){
      warning("The output object has not been generated for any of the studies!")
    }
  }
  
}
