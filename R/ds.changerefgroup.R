#' 
#' @title changes a reference level of a factor
#' @description this function is similar to R function \code{relevel}, but in addition addes numbering to the levels
#' so that they are displayed in the right order when creating cross-tables.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @param xvect a factor
#' @param ref the reference level
#' @param newobj the name of the new variable. If this argument is set to NULL, the name of the new 
#' variable is the name of the input variable with the suffixe '_newref' (e.g. 'PM_BMI_CATEGORICAL_newref', if input 
#' variable's name is 'PM_BMI_CATEGORICAL')
#' @return a factor of the same length as xvect
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
#' # rename the levels of PM_BMI_CATEGORICAL and make "obesity" as a reference level
#' ds.recodelevels(opals, quote(D$PM_BMI_CATEGORICAL), newlabels=c('normal', 'overweight', 'obesity'), 'bmi_new')
#' ds.changerefgroup(opals, quote(bmi_new), ref='2_obesity', newobj = 'bmi_ob')
#' ds.levels(opals, quote(bmi_ob))
#' 
#' # or without renaming the levels (group "3" as a reference level)
#' ds.changerefgroup(opals, quote(D$PM_BMI_CATEGORICAL), ref='3')
#' ds.levels(opals, quote(PM_BMI_CATEGORICAL_newref))
#' 
#' }
#' 
ds.changerefgroup = function(datasources=NULL, xvect=NULL, ref=NULL, newobj=NULL){
  
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
    newobj <- paste0(varname, "_newref")
  }
  
  # call the server side function that will recode the levels
  cally <- call('changerefgroup.ds', xvect, ref )
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
