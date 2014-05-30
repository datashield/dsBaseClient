#' 
#' @title Combines objects by columns
#' @description this is similar to the R base function 'cbind'
#' @details the function combines vectors or vectors and matrices or dataframes by columns.
#' But unlike the R base function 'cbind' the output is a dataframe.
#' @param objects a vector which holds the names of the objects to combine
#' @param newobj the name of the output object. If this argument is set to \code{NULL}, 
#' the name of the new object is 'newDataframe'.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @return nothing is returned to the client, the new object is stored on the server side.
#' @author Gaye, A.
#' @export
#' @examples {
#' 
#' # load the file that contains the login details
#' data(logindata)
#' 
#' # login and assign the required variables to R
#' myvar <- list('LAB_TSC', 'LAB_HDL')
#' opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#' # generate a new dataframe by combining the log values of 'LAB_TSC' and 'LAB_HDL', by columns
#' ds.assign('labtsc', 'log(D$LAB_TSC)')
#' ds.assign('labhdl', 'log(D$LAB_HDL)')
#' myobjects <- c('labtsc', 'labhdl')
#' ds.cbind(objects=myobjects)
#' 
#' }
#' 
ds.cbind = function(objects=NULL, newobj=NULL, datasources=NULL){
  
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
    message(" Please provide the list of objects to combine.")
    stop(" End of process!\n", call.=FALSE)
  }
  
  if(length(objects) < 2){
    stop(" You must provide a list of at least to objects to combine!\n", call.=FALSE)
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
    
  if(is.null(newobj)){
    newobj <- 'newDataframe'
  }
  
  # call the server side function
  cally <-  paste0("cbindDS(list(","'",paste(objects,collapse="','"),"'","))")
  datashield.assign(datasources, newobj, as.symbol(cally))
  
  # check that the new object has been created and display a message accordingly
  finalcheck <- isAssigned(datasources, newobj)
  
}