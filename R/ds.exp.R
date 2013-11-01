#' 
#' @title Computes the exponential function
#' @description This function is similar to R function \code{exp}. 
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @param xvect a vector.
#' @param newobj the name of the new vector.If this argument is set to \code{NULL}, the name of the new 
#' variable is the name of the input variable with the suffixe '_exp' (e.g. 'PM_BMI_CONTINUOUS_exp', if input 
#' variable's name is 'PM_BMI_CONTINUOUS')
#' @return a message is displayed when the action is completed.
#' @author Gaye, A. (amadou.gaye@bristol.ac.uk) and Isaeva, J. (julia.isaeva@fhi.no)
#' @export
#' @examples {
#' 
#' # load that contains the login details
#' data(logindata)
#' 
#' # login and assign specific variable(s)
#' myvar <- list("PM_BMI_CONTINUOUS")
#' opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#' # compute exponential function of the 'PM_BMI_CONTINUOUS' variable
#' ds.exp(datasources=opals, xvect=quote(D$PM_BMI_CONTINUOUS))
#' 
#' }
#' 
ds.exp = function(datasources=NULL, xvect=NULL, newobj=NULL){
  
  if(is.null(datasources)){
    message("\n ALERT!\n")
    message(" No valid opal object(s) provided.")
    message(" Make sure you are logged in to valid opal server(s).")
    stop(" End of process!\n", call.=FALSE)
  }
  
  if(is.null(xvect)){
    message("\n ALERT!\n")
    message(" Please provide a valid numeric vector.")
    stop(" End of process!\n", call.=FALSE)
  }
  
  # the elements in the argument passed on as a call
  elements <- unlist(strsplit(deparse(vector), split=c("\\,")))
  numelts <- length(elements)
  # get the names of the variables in the 'call' argument
  myvars <- c()
  for(i in 1:numelts){
    if(i == 1){
      temp <- unlist(strsplit(elements[i], split="\\("))
      myvars <- append(myvars, unlist(strsplit(temp, split=" "))[[2]])
    }else{
      if(i < numelts){
        temp <- unlist(strsplit(elements[i], split="\\,"))
        myvars <- append(myvars, unlist(strsplit(temp, split=" "))[[2]])
      }else{
        temp <- unlist(strsplit(elements[i], split="\\)"))
        myvars <- append(myvars, unlist(strsplit(temp, split=" "))[[2]])
      }
    }
  }
  # if there is only one variable i.e. then we need to get rid of the trailing ')'
  if(length(myvars) < 2){ myvars <- unlist(strsplit(myvars, split="\\)")) }
  
  # call the function that checks that the object are defined.
  # If the objects are within a dataframe we check if the dataframe exists and if they are
  # 'loose' objects stored in the server like variables not attached to a dataframe then we 
  # check if the variable is present in the servers
  flag <- c()
  for(q in 1:length(myvars)){
    obj <- myvars[[q]]
    inputterms <- unlist(strsplit(obj, "\\$", perl=TRUE))
    
    if(length(inputterms) > 1){
      dframe <-  unlist(strsplit(obj, "\\$", perl=TRUE))[[1]][1]
      for(i in 1:length(datasources)){
        out <- c()
        cally <- call('exists', dframe )
        qc <- datashield.aggregate(datasources[i], cally)
        out <- append(out, qc[[1]])
        xx <- which(out == FALSE)
        if(length(xx) > 0){
          warning("The table, '", dframe, "', is not defined in ", paste0(names(datasources), collapse=","), "!")
          flag <- append(flag, i)
        }
      }
    }else{
      objname <-  deparse(obj)
      for(i in 1:length(datasources)){
        out <- c()
        cally <- call('exists', objname)
        qc <- datashield.aggregate(datasources[i], cally)
        out <- append(out, qc[[1]])
        xx <- which(out == FALSE)
        if(length(xx) > 0){
          warning("The object, '", objname, "', is not defined in ", paste0(names(datasources), collapse=","), "!")
          flag <- append(flag, i)
        }
      }
      
    }
  }
  
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
    newobj <- paste0(varname, "_exp")
  }
  
  # call the server side function that does the job
  cally <- call('exp', xvect )
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