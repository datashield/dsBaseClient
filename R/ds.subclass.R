#' 
#' @title Generates valid subset(s) of a dataframe or a factor
#' @description The function takes a categorical vector or dataframe as input and generates subset(s)
#' vectors or dataframes for each category. Subsets are considered invalid if they hold between 1 and 
#' 4 observations.
#' @details If the input data object is a dataframe it is possible to specify  the variables  
#' to subset on. If a subset is not 'valid' all its the values are reported as missing (i.e. NA),
#' the name of the subsets is labelled as '_INVALID'.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @param subsets the name of the output object, a list that holds the subset objects. If set to NULL
#' the default name of this list is 'subsclasses' 
#' @param data a string character, the name of the dataframe or the vector to generate subsets from.
#' @param variables a vector of string characters, the name(s) of the variables to subset by.
#' @return a message is displayed when the action is completed.
#' @author Gaye, A.
#' @export
#' @examples {
#'
#' # load the login data
#' library(opal)
#' data(logindata)
#' 
#' # login and assign some variables to R
#' myvar <- list("DIS_DIAB","PM_BMI_CONTINUOUS","LAB_HDL", "GENDER")
#' opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#' # Example 1: generate all possible subsets from the table assigne above 
#' ds.subclass(datasources=opals, subsets="subtables", data="D")
#' 
#' # Example 2: subset the table initially assigned by the variable 'GENDER'
#' ds.subclass(datasources=opals, subsets="subtables", data="D", variables="GENDER")
#' 
#' # Example 3: generate a new variable 'gender' and split it into two vectors: males and females
#' datashield.assign(opals, "gender", quote(D$GENDER))
#' ds.subclass(datasources=opals, subsets="mf.tables", data="gender")
#' 
#' ds.subclass(datasources=opals, subsets="subvectors", data=NULL, variables=NULL)
#' }
#' 
ds.subclass <- function(datasources=NULL, subsets="subsclasses", data=NULL, variables=NULL){
  
  if(is.null(datasources)){
    message("No valid opal object(s) provided!")
    message("Make sure you are logged in to valid opal server(s).")
    stop(" End of process!\n", call.=FALSE)
  }
  
  # call the server side function that does the job
  # get the indices of the columns refered to by their names in the arguments
  cally <- call('subclassDS', data, variables)
  datashield.assign(datasources, subsets, cally)
  
  # check the subsets and tell if some are invalid 
  
  
  # a message so the user knows the function was ran (assign functions are 'silent')
  message("An 'assign' function was ran, no output should be expected on the client side!")
  
  # check that the new object has been created and display a message accordingly
  tell <- dsbaseclient:::.ds.tellUser(datasources, subsets)
}
