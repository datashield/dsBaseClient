#' 
#' @title Generates a valid binary variable from a continuous variable
#' @details This function runs only if the set threshold criterion is met or not met by more 
#' than one observation. The observation that met the criterion as assigned the value '1'
#' and other onservations are assigned '0'
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @param xvect a numeric vector.
#' @param operator an string character: '>', '<', '>=', '<=', '==' or '!='.
#' @param threshold a numeric value that sets the threshold level.
#' @param newobj the name of the output object. If this argument is set to NULL, 
#' the name of the new object is the name of the input variable with a suffixe '_subset'.
#' @return a message is displayed when the action is completed.
#' @author Gaye, A. (amadou.gaye@bristol.ac.uk)
#' @export
#' @examples {
#' 
#' # load the login data
#' data(logindata)
#' 
#' # login and assign some variables to R
#' myvar <- list("PM_BMI_CONTINUOUS", "LAB_HDL")
#' opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#' # generate a binary variable variable using the threshold bmi >= 25
#' ds.subsetvar(datasources=opals, xvect=quote(D$PM_BMI_CONTINUOUS), operator=">=", threshold=25)
#' 
#' }
#' 
ds.subsetvar = function(datasources=NULL, xvect=NULL, threshold=NULL, operator=NULL, newobj=NULL){
  
  if(is.null(datasources)){
    message("\n ALERT!\n")
    message(" No valid opal object(s) provided.")
    message(" Make sure you are logged in to valid opal server(s).\n")
    stop(" End of process!\n", call.=FALSE)
  }
  
  if(is.null(xvect)){
    message("\n ALERT!\n")
    message(" Please provide a valid numeric vector.")
    stop(" End of process!\n", call.=FALSE)
  }
  
  if(is.null(threshold)){
    message("\n ALERT!\n")
    message(" Please provide a threshold value.")
    stop(" End of process!\n", call.=FALSE)
  }
  
  if(is.null(operator)){
    message("\n ALERT!\n")
    message(" Please provide a logical operator")
    stop(" End of process!\n", call.=FALSE)
  }else{
    operators <- c(">", "<", ">=", "<=", "==", "!=")
    if(!(operator %in% operators)){
      stop(" Please provide a valid logical operator: '>', '<', '>=', '<=', '==' or '!='.")
    }
  }
    
  # the input variable might be given as column table (i.e. D$xvect)
  # or just as a vector not attached to a table (i.e. xvect)
  # we have to make sure the function deals with each case
  inputterms <- unlist(strsplit(deparse(xvect), "\\$", perl=TRUE))
  if(length(inputterms) > 1){
    var <-  strsplit(deparse(xvect), "\\$", perl=TRUE)[[1]][2]
  }else{
    var <- deparse(xvect)
  }
  
  # create a name by default if user did not provide a name for the new variable
  if(is.null(newobj)){
    newobj <- paste(var, "_subset", sep="")
  }

  # set the integer that is passed to the server side function to indicate a logical operator
  if(operator == ">"){
    operator <- 1
  }
  if(operator == "<"){
    operator <- 2
  }
  if(operator == ">="){
    operator <- 3
  }
  if(operator == "<="){
    operator <- 4
  }
  if(operator == "=="){
    operator <- 5
  }
  if(operator == "!="){
    operator <- 6
  }
  
  # call the server side function that does the job
  cally <- call('subsetvar.ds', xvect, operator, threshold)
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
