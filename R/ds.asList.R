#' 
#' @title Constructs an object of type list
#' @description This function is similar to R function \code{as.list}. 
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @param x an object to be converted into a list
#' @param newobj the name of the new vector.If this argument is set to NULL, the name of the new 
#' variable is the name of the input variable with the suffixe '_list' (e.g. 'D_factor', if input 
#' variable's name is 'D')
#' @return a message is displayed when the action is completed.
#' @author Gaye, A. (amadou.gaye@bristol.ac.uk) and Isaeva, J. (julia.isaeva@fhi.no)
#' @export
#' @examples {
#' 
#' # load that contains the login details
#' data(logindata)
#' 
#' # login and assign specific variable(s)
#' opals <- datashield.login(logins=logindata,assign=TRUE)
#' 
#' # turn the data frame D into a list
#' ds.asList(datasources=opals, x=quote(D))
#' }
#' 
ds.asList = function(datasources=NULL, x=NULL, newobj=NULL){
  
  if(is.null(datasources)){
    message("\n ALERT!\n")
    message(" No valid opal object(s) provided.")
    message(" Make sure you are logged in to valid opal server(s).")
    stop(" End of process!\n", call.=FALSE)
  }
  
  if(is.null(x)){
    message("\n ALERT!\n")
    message(" Please provide a valid input.")
    stop(" End of process!\n", call.=FALSE)
  }
  
  #   # call the function that checks that the object are defined.
  #   # If the objects are within a dataframe we check if the dataframe exists and if they are
  #   # 'loose' objects stored in the server like variables not attached to a dataframe then we 
  #   # check if the variable is present in the servers
  #   flag <- c()
  #   for(q in 1:length(x)){
  #     obj <- x[[q]]
  #     inputterms <- unlist(strsplit(deparse(obj), "\\$", perl=TRUE))
  #     
  #     if(length(inputterms) > 1){
  #       dframe <-  unlist(strsplit(deparse(obj), "\\$", perl=TRUE))[[1]][1]
  #       for(i in 1:length(datasources)){
  #         out <- c()
  #         cally <- call('exists', dframe )
  #         qc <- datashield.aggregate(datasources[i], cally)
  #         out <- append(out, qc[[1]])
  #         xx <- which(out == FALSE)
  #         if(length(xx) > 0){
  #           warning("The table, '", dframe, "', is not defined in ", paste0(names(datasources), collapse=","), "!")
  #           flag <- append(flag, i)
  #         }
  #       }
  #     }else{
  #       objname <-  deparse(obj)
  #       for(i in 1:length(datasources)){
  #         out <- c()
  #         cally <- call('exists', objname)
  #         qc <- datashield.aggregate(datasources[i], cally)
  #         out <- append(out, qc[[1]])
  #         xx <- which(out == FALSE)
  #         if(length(xx) > 0){
  #           warning("The object, '", objname, "', is not defined in ", paste0(names(datasources), collapse=","), "!")
  #           flag <- append(flag, i)
  #         }
  #       }
  #       
  #     }
  #  }
  
  # the input variable might be given as column table (i.e. D$x)
  # or just as a vector not attached to a table (i.e. x)
  # we have to make sure the function deals with each case
  inputterms <- unlist(strsplit(deparse(x), "\\$", perl=TRUE))
  if(length(inputterms) > 1){
    varname <- strsplit(deparse(x), "\\$", perl=TRUE)[[1]][2]
  }else{
    varname <- deparse(x)
  }
  
  # create a name by default if user did not provide a name for the new variable
  if(is.null(newobj)){
    newobj <- paste0(varname, "_list")
  }
  
  # call the server side function that does the job
  cally <- call('as.list', x )
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