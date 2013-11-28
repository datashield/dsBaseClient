#' 
#' @title recodes a categorical variable
#' @description this function recodes levels of a categorical variable with new given labels. 
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @param xvect a factor
#' @param newlabels a string vector with new labels for the levels
#' If \code{newlabels} is not specified, the naming of original levels is amended - numbering is added (0_..., 1_..., 2_... etc.)
#' @param newobj the name of the new variable. If this argument is set to NULL, the name of the new 
#' variable is the name of the input variable with the suffixe '_recoded' (e.g. 'PM_BMI_CATEGORICAL_recoded', if input 
#' variable's name is 'PM_BMI_CATEGORICAL')
#' @return a factor vector with new labels for levels
#' @author Isaeva, I.; Gaye, A.
#' @export
#' @examples {
#' 
#' # load that contains the login details
#' data(logindata)
#' 
#' # login and assign specific variable(s)
#' myvar <- list("PM_BMI_CATEGORICAL")
#' opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#' # rename the levels of PM_BMI_CATEGORICAL
#' ds.recodelevels(opals, quote(D$PM_BMI_CATEGORICAL), newlabels=c('normal', 'overweight', 'obesity'), 'bmi_new')
#' ds.levels(opals, quote(bmi_new))
#' 
#' }
#' 
ds.recodelevels = function(datasources=NULL, xvect=NULL, newlabels=NULL, newobj=NULL){
  
  if(is.null(datasources)){
    message("\n\n ALERT!\n")
    message(" No valid opal object(s) provided.\n")
    message(" Make sure you are logged in to valid opal server(s).\n")
    stop(" End of process!\n\n", call.=FALSE)
  }
  
  if(is.null(xvect)){
    message("\n\n ALERT!\n")
    message(" Please provide a valid factor vector\n")
    stop(" End of process!\n\n", call.=FALSE)
  }
  
  # call the function that checks the variable is available and not empty
  vars2check <- list(xvect)
  datasources <- ds.checkvar(datasources, vars2check)
  
  # the input variable might be given as column table (i.e. D$xvect)
  # or just as a vector not attached to a table (i.e. xvect)
  # we have to make sure the function deals with each case
  inputterms <- unlist(strsplit(deparse(xvect), "\\$", perl=TRUE))
  if(length(inputterms) > 1){
    varname <- strsplit(deparse(xvect), "\\$", perl=TRUE)[[1]][2]
  }else{
    varname <- deparse(xvect)
  }
  
  # create a name by default if user did not provide a name for the new variable
  if(is.null(newobj)){
    newobj <- paste0(varname, "_recoded")
  }
  
  # call the server side function that will recode the levels
  cally <- call('recodelevels.ds', xvect, newlabels )
  datashield.assign(datasources, newobj, cally)
  
  
  # a message so the user know the function was run (assign function are 'silent')
  message("An 'assign' function was run, no output should be expected on the client side!")
  
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
