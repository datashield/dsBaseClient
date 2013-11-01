#' 
#' @title Combines values into a vector or list
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @param vector a vector that holds the objects to combine
#' @param newobj the name of the output object. If this argument is set to NULL, 
#' the name of the new object is 'cvect'.
#' @return  a message is displayed when the action is completed.
#' @author Gaye, A. (amadou.gaye@bristol.ac.uk)
#' @export
#' @examples {
#' 
#' # load the file that contains the login details
#' data(logindata)
#' 
#' # login and assign the required variables to R
#' myvar <- list("LAB_TSC","LAB_HDL")
#' opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#' # compute the product of 'LAB_TSC' by 'LAB_HDL' and assign it to 'P'
#' myvect <- quote(c(D$LAB_TSC, D$LAB_HDL))
#' ds.c(datasources=opals, vector=myvect)
#' }
#' 
ds.c = function(datasources=NULL, vector=NULL, newobj=NULL){
  
  if(is.null(datasources)){
    message("\n ALERT!\n")
    message(" No valid opal object(s) provided.")
    message(" Make sure you are logged in to valid opal server(s).\n")
    stop(" End of process!\n", call.=FALSE)
  }
  
  if(is.null(vector)){
    message("\n ALERT!\n")
    message(" Please provide the names of the objects to combine.")
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
 
  # create a name by default if user did not provide a name for the new variable
  if(is.null(newobj)){
    newobj <- "cvect"
  }
  
  # call the server side function that does the job: do nothing if none of studies passed the checks above;
  # run the server side function only for the studies that passed the checks
  if(length(flag) == length(datasources)){
    stop("One or more of the objects to combine are not defined in any of the studies servers!")
  }else{
    
    if(is.null(flag)){
      # this will call the c function defined on the server side
      cally <- as.call(vector)
      datashield.assign(datasources, newobj, cally)
    }else{
      cally <- as.call(vector)
      datasources <- datasources[-flag]
      datashield.assign(datasources, newobj, cally)
      message("The objects were combined only  for ", paste0(names(datasources), collapse=","), ".")
    }
  }
  
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
