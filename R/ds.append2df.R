#' 
#' @title Appends a vector/column to a dataframe
#' @details The function appends a numeric or factor vector to a dataframe. The initial 
#' dataframe is replaced by the new dataframe if the argument 'replace' is set to 'TRUE';
#' otherwise a new dataframe is generated and named after the intitial dataframe with the 
#' addition of a suffixe 'new'; for example if the initial dataframe is 'D' the new dataframe 
#' will be 'Dnew'.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @param dataset the input dataset, the table to append a column to.
#' @param xvect a numeric or factor vector
#' @param replace a character which tells if the intitial dataframe should be replaced by the 
#' the newly dataframe (i.e. the one with the additional column).
#' If \code{replace} is set to 'FALSE', default, the initial dataframe is not replaced and the 
#' new dataframe is named as exaplained in the 'details' section of the documentation.
#' if \code{replace} is set to 'TRUE', the initial dataframe is replaced (i.e. the new dataframe 
#' takes the name of the initial one)
#' @return a message is displayed when the action is completed.
#' @author Gaye, A.
#' @export
#' @examples {
#' 
#' # load that contains the login details
#' data(logindata)
#' 
#' # login and assign specific variable(s)
#' myvar <- list("LAB_HDL","LAB_TSC")
#' opals <- datashield.login(logins=logindata, assign=TRUE, variables=myvar, symbol="D")
#' 
#' # generate a new variable (e.g. a mean centered LAB_HDL)
#' # get the mean of LAB_HDL
#' mean.lab_hdl <- ds.mean(opals, 'D$LAB_HDL', type='split')
#' # center LAB_HDL for each study
#' for(i in 1:length(opals)){
#'   call.object <- call("-", quote(D$LAB_HDL), mean.lab_hdl[[i]])
#'   datashield.assign(opals[i], "lab_hdl.c", call.object)
#' }
#' 
#' # Example 1: append the variable 'lab_hdl.c' to 'D' and generate 'Dnew'
#' ds.append2df(datasources=opals, quote(D), quote(lab_hdl.c))
#' 
#' # Example 2: append the variable 'lab_hdl.c' to 'D' and update 'D'
#' ds.append2df(datasources=opals, dataset=quote(D), xvect=quote(lab_hdl.c), replace=quote(TRUE))
#' }
#' 
ds.append2df = function(datasources=NULL, dataset=NULL, xvect=NULL, replace=FALSE){
  
  if(is.null(datasources)){
    message(" ALERT!")
    message(" No valid opal object(s) provided.")
    message(" Make sure you are logged in to valid opal server(s).")
    stop(" End of process!", call.=FALSE)
  }
  
  if(is.null(dataset)){
    message(" ALERT!")
    message(" No dataframe provided - check the argument 'dataset'.")
    stop(" End of process!", call.=FALSE)
  }
    
  if(is.null(xvect)){
    message(" ALERT!")
    message(" Please provide a vector to append to the dataframe.")
    stop(" End of process!", call.=FALSE)
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
  
  # call the server side function that does the business
  # replace the input dataset or not depending on the value of 'replace'
  cally <- call("append2df.ds", dataset, xvect, quote(var))
  newname <- paste(datasetname, "new", sep="")
  if(replace){
    datashield.assign(datasources, newname, cally)  
    newdataset <- call(newname)[[1]]
    datashield.assign(datasources, datasetname, newdataset)
    # the assignment in the below line is just to avoid output printed to screen
    a <- datashield.rm(datasources, newdataset)
  }else{
    datashield.assign(datasources, newname, cally)  
  }
}