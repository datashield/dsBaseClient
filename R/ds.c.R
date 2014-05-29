#' 
#' @title Combines values into a vector or list
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @param vect a character vector that holds the names of the objects to combine.
#' @param newobj the name of the output object. If this argument is set to NULL, 
#' the name of the new object is 'cvect'.
#' @return  a message is displayed when the action is completed.
#' @author Gaye, A.
#' @export
#' @examples {
#' 
#' # load the file that contains the login details
#' data(logindata)
#' 
#' # login and assign the required variables to R
#' myvar <- c('LAB_TSC', 'LAB_HDL')
#' opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#' # Get the variables 'LAB_TSC' by 'LAB_HDL' from the dataframe 'D' and combine them
#' myvect <- c('D$LAB_TSC', 'D$LAB_HDL')
#' ds.assign('labtsc', 'D$LAB_TSC')
#' ds.assign('labhdl', 'D$LAB_HDL')
#' myvect <- c('labtsc', 'labhdl')
#' ds.c(vect=myvect)
#' }
#' 
ds.c = function(vect=NULL, newobj=NULL, datasources=NULL){
  
  if(is.null(datasources)){
    findLogin <- getOpals()
    if(findLogin$flag == 1){
      datasources <- findLogin$opals
    }else{
      if(findLogin$flag == 0){
        stop(" Are yout logged in to any server? Please provide a valid opal login object! ", call.=FALSE)
      }else{
        message(paste0("More than one list of opal login object were found: '", paste(findLogin$opals,collapse="', '"), "'!"))
        stop(" Please set the parameter 'datasources' to the list you want to use. ", call.=FALSE)
      }
    }
  }
  
  if(is.null(vect)){
    message("\n ALERT!\n")
    message(" Please provide the names of the objects to coerce.")
    stop(" End of process!\n", call.=FALSE)
  }
  
  # the input variables might be given as column table (i.e. D$xvect)
  # or just as a vector not attached to a table (i.e. xvect)
  # we have to make sure the function deals with each case
  vct2 <- c()
  flag <- FALSE
  for(i in 1:length(vect)){
    inputterms <- unlist(strsplit(vect[i], "\\$", perl=TRUE))
    if(length(inputterms) > 1){
      varname <- strsplit(vect[i], "\\$", perl=TRUE)[[1]][2]
      flag <- TRUE
    }
  }
  
  # check if the input object(s) is(are) defined in all the studies
  if(flag) {
    message("'$' sign encountered in the name of one or more of the input objects to combine.")
    stop("The objects to combine should be within a structure!", call.=FALSE)
  }else{
    defined <- isDefined(datasources,vect)
  }
  
  # create a name by default if user did not provide a name for the new variable
  if(is.null(newobj)){
    newobj <- "cvect"
  }
  
  # call the server side function that does the job
  cally <-  paste0("cDS(list(","'",paste(myvect,collapse="','"),"'","))")
  datashield.assign(datasources, newobj, as.symbol(cally))
  
  # check that the new object has been created and display a message accordingly
  cally <- call('exists', newobj)
  qc <- datashield.aggregate(datasources, cally)
  indx <- as.numeric(which(qc==TRUE))
  
  if(length(indx) > 0 & length(indx) < length(datasources)){
    stop("The output object, '", newobj, "', was generated only for ", names(datasources)[indx], "!", call.=FALSE)
  }
  if(length(indx) == 0){
    stop("The output object has not been generated for any of the studies!", call.=FALSE)
  }
  
}
