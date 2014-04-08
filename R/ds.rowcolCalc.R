#' 
#' @title Computes sums and means of rows or columns of numeric arrays
#' @details The function is similar to R base functions 'rowSums', 'colSums',
#' 'rowMeans' and 'colMeans'. It is an assign function which means its outcome
#' is not returned to the user as it might be revealing if the array contains 
#' only one row or one column. 
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @param dataset an array of two or more dimensions.
#' @param operation a character string which indicates the operation to carry out:
#' "rowSums", "colSums", "rowMeans" or "colMeans".
#' @param newobj the name of the new object. If this argument is set to NULL, the name of the new 
#' variable, set by default, is 'rowcolCalc_out'.
#' @return a message is displayed when the action is completed.
#' @export
#' @author Gaye, A.
#' @examples {
#' 
#' # load that contains the login details
#' data(logindata)
#' 
#' # login and assign the whole dataset on the opal server
#' opals <- datashield.login(logins=logindata,assign=TRUE)
#' 
#' # create an array using the variables "LAB_HDL" and"LAB_TSC" which we know are both numeric vectors 
#' datashield.assign(opals, "hdl_tsc", quote(data.frame(cbind(D$LAB_HDL, D$LAB_TSC))))
#' 
#' # now turn the newly created vector 'gender_ch' into a numeric
#' ds.rowcolCalc(datasources=opals, dataset=quote(hdl_tsc), operation="rowSums", newobj="rsum_hdl_tsc")
#' 
#' }
#' 
ds.rowcolCalc = function(datasources=NULL, dataset=NULL, operation=NULL, newobj=NULL){
  
  if(is.null(datasources)){
    message(" ALERT!")
    message(" No valid opal object(s) provided.")
    message(" Make sure you are logged in to valid opal server(s).")
    stop(" End of process!", call.=FALSE)
  }
  
  if(is.null(dataset)){
    message(" ALERT!")
    message(" Please provide a valid dataset")
    stop(" End of process!", call.=FALSE)
  }else{
    # check that, for each study,  all the columns of the input table are of 'numeric' type
    message("Checking if all the columns on the input table are numeric.")
    numsources <- length(datasources)
    stdnames <- names(datasources)
    dtname <- as.character(dataset)
    for(i in 1:numsources){
      dims <- datashield.aggregate(datasources[i],  paste0("dim(", dataset, ")"))
      cols <- datashield.aggregate(datasources[i], paste0("colnames(", dataset, ")"))
      for(j in 1:dims[[1]][2]){
        cally <- paste0("is.numeric(", dtname, "$", cols[[1]][j], ")")
        res <- datashield.aggregate(datasources[i], cally)
        if(!(res[[1]])){
         stop(" One or more columns of ", dtname, " are not of numeric type, in ",  stdnames[i])
        }
      }
    }
  }
  
  ops <- c("rowSums","colSums","rowMeans","colMeans")
  if(is.null(operation)){
    message(" ALERT!")
    message(" Please indicate the calculation required.")
    stop(" End of process!", call.=FALSE)
  }else{
    if(!(operation %in% ops)){
      stop(" 'operation' must be: 'rowSums', 'colSums', 'rowMeans' or 'colMeans'")
    }
  }
  
  # operation to carry out
  indx <- which(ops == operation)
  
  # create a name by default if user did not provide a name for the new variable
  if(is.null(newobj)){
    newobj <- "rowcolCalc_out"
  }
  
  # call the server side function that does the job
  cally <-  call("rowcolCalc.ds", dataset, as.numeric(indx))
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
