#' 
#' @title Combines values into a vector or list
#' @param objects a character vector that holds the names of the objects to combine.
#' @param newobj the name of the output object. If this argument is set to NULL, 
#' the name of the new object is 'newObject'.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @return  nothing is returned to the client, the new object is stored on the server side.
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
#' ds.c(objects=myvect)
#' }
#' 
ds.c = function(objects=NULL, newobj=NULL, datasources=NULL){
  
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
  
  if(is.null(objects)){
    message("\n ALERT!\n")
    message(" Please provide the names of the objects to coerce.")
    stop(" End of process!\n", call.=FALSE)
  }
  
  # the input variables might be given as column table
  # or just as a vector not attached to a table
  # we have to make sure the function deals with each case
  vct2 <- c()
  flag <- FALSE
  for(i in 1:length(objects)){
    inputterms <- unlist(strsplit(objects[i], "\\$", perl=TRUE))
    if(length(inputterms) > 1){
      varname <- strsplit(objects[i], "\\$", perl=TRUE)[[1]][2]
      flag <- TRUE
    }
  }
  
  # check if the input object(s) is(are) defined in all the studies
  if(flag) {
    message("'$' sign encountered in the name of one or more of the input objects to combine.")
    stop("The objects to combine should be within a structure!", call.=FALSE)
  }else{
    defined <- isDefined(datasources, objects)
  }
  
  # call the internal function that checks the input object(s) is(are) of the same class in all studies.
  for(i in 1:length(objects)){
    typ <- checkClass(datasources, objects[i])
  }
  
  # create a name by default if user did not provide a name for the new variable
  if(is.null(newobj)){
    newobj <- 'newObject'
  }
  
  # call the server side function that does the job
  cally <-  paste0("cDS(list(","'",paste(objects,collapse="','"),"'","))")
  datashield.assign(datasources, newobj, as.symbol(cally))
  
  # check that the new object has been created and display a message accordingly
  finalcheck <- isAssigned(datasources, newobj)
  
}
