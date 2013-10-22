#' 
#' @title Append a vector/column to a dataframe
#' @details The function appends a numeric or factor vector to a dataframe. The initial 
#' dataframe is replaced by the new dataframe if the argument 'replace' is set to 'TRUE';
#' otherwise a new dataframe is generated and named after the intitial dataframe with the 
#' addition of a suffixe 'new'; for example if the initial dataframe is 'D' the new dataframe 
#' will be 'Dnew'.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @param xvect a numeric or factor vector
#' @param replace a character which tells if the intitial dataframe should be replaced by the 
#' the newly dataframe (i.e. the one with the additional column).
#' If \code{replace} is set to 'FALSE', default, the initial dataframe is not replaced and the 
#' new dataframe is named as exaplained in the 'details' section of the documentation.
#' if \code{replace} is set to 'TRUE', the initial dataframe is replaced (i.e. the new dataframe 
#' takes the name of the initial one)
#' @return a dataframe
#' @author Gaye, A.
#' @export
#' @examples {
#' 
#' # load that contains the login details
#' data(logindata)
#' 
#' # login and assign specific variable(s)
#' myvar <- list("LAB_HDL")
#' opals <- datashield.login(logins=logindata, assign=TRUE, variables=myvar, symbol="D")
#' 
#' # generate a new variable (e.g. a mean centered LAB_HDL)
#' # get the mean of LAB_HDL
#' mean.lab_hdl <- ds.mean(opals, quote(D$LAB_HDL), type='split')
#' # center LAB_HDL for each study
#' for(i in 1:length(opals)){
#'   call.object <- call("-", quote(D$LAB_HDL), mean.lab_hdl[[i]])
#'   datashield.assign(opals[i], "lab_hdl.c", call.object)
#' }
#' 
#' # Example 1: append the variable 'LAB_TSC' to 'D' and generate 'Dnew'
#' ds.append2df(datasources=opals, quote(D), quote(lab_hdl.c))
#' 
#' # Example 2: append the variable 'LAB_TSC' to 'D' and replace 'D' by the new dataframe
#' ds.append2df(datasources=opals, dataset=quote(D), xvect=quote(D$LAB_TSC), replace=quote(TRUE))
#' }
#' 
ds.append2df = function(datasources=NULL, dataset=NULL, xvect=NULL, replace=FALSE){
  
  if(is.null(datasources)){
    cat("\n\n ALERT!\n")
    cat(" No valid opal object(s) provided.\n")
    cat(" Make sure you are logged in to valid opal server(s).\n")
    stop(" End of process!\n\n", call.=FALSE)
  }
  
  if(is.null(dataset)){
    cat("\n\n ALERT!\n")
    cat(" No dataframe provided - check the argument 'dataset'.\n")
    stop(" End of process!\n\n", call.=FALSE)
  }
    
  if(is.null(xvect)){
    cat("\n\n ALERT!\n")
    cat(" Please provide a vector to append to the dataframe.\n")
    stop(" End of process!\n\n", call.=FALSE)
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
  
  # get the name of the dataframe
  datasetname <-  as.character(dataset)
  
  # call the function that checks the variable is available and not empty
  vars2check <- list(xvect)
  datasources <- ds.checkvar(datasources, vars2check)
  
  # name of the studies
  stdnames <- names(datasources)
  
  # number of studies
  num.sources <- length(datasources)
  
  # call the server side function that does the business
  # replace the input dataset or not depending on the value of 'replace'
  call.object <- call("append2df.ds", dataset, xvect, quote(var))
  newname <- paste(datasetname, "new", sep="")
  if(replace){
    datashield.assign(opals, newname, call.object)  
    symbol <- call(newname)[[1]]
    datashield.assign(opals, datasetname, quote(symbol))  
    datashield.rm(opals, newname)
  }else{
    datashield.assign(opals, newname, call.object)  
  }
}