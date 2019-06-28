#' 
#' @title Combines values into a vector or list
#' @description Concatenates object into one object.
#' @details To avoid combining the character names and not the vectors 
#' on the client side, the names are coerce into a list and the server side
#' function loops through that list to concatenate the list's elements into a vector.
#' @param x a character, a vector that holds the names of the objects to combine.
#' @param newobj the name of the output object. If this argument is set to NULL, 
#' the name of the new object is 'newObject'.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @return  nothing is returned to the client, the new object is stored on the server side.
#' @author Gaye, A.
#' @export
#' @examples
#' \dontrun{
#' 
#'   # load the file that contains the login details
#'   data(logindata)
#' 
#'   # login and assign specific variable(s)
#'   # (by default the assigned dataset is a dataframe named 'D')
#'   myvar <- c('LAB_TSC', 'LAB_HDL')
#'   opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#'   # Get the variables 'LAB_TSC' by 'LAB_HDL' from the dataframe 'D' and combine them
#'   myvect <- c('D$LAB_TSC', 'D$LAB_HDL')
#'   ds.assign(toAssign='D$LAB_TSC', newobj='labtsc')
#'   ds.assign(toAssign='D$LAB_HDL', newobj='labhdl')
#'   myvect <- c('labtsc', 'labhdl')
#'   ds.c(x=myvect)
#' 
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(opals)
#' 
#' }
#' 
ds.c <- function(x=NULL, newobj=NULL, datasources=NULL){
  
  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }
  
  if(is.null(x)){
    stop("x=NULL. Please provide the names of the objects to concatenate!", call.=FALSE)
  }
  
  # the input variable might be given as column table (i.e. D$object)
  # or just as a vector not attached to a table (i.e. object)
  # we have to make sure the function deals with each case
  xnames <- extract(x)
  varnames <- xnames$elements
  obj2lookfor <- xnames$holders
  
  # check if the input object(s) is(are) defined in all the studies
  for(i in 1:length(varnames)){
    if(is.na(obj2lookfor[i])){
      defined <- isDefined(datasources, varnames[i])
    }else{
      defined <- isDefined(datasources, obj2lookfor[i])
    }
  }
  
  # call the internal function that checks the input object(s) is(are) of the same class in all studies.
  for(i in 1:length(x)){
    typ <- checkClass(datasources, x[i])
  }
  
  # create a name by default if user did not provide a name for the new variable
  if(is.null(newobj)){
    newobj <- 'newObject'
  }
  
  # call the server side function that does the job
  cally <-  paste0("cDS(list(",paste(x,collapse=","),"))")
  opal::datashield.assign(datasources, newobj, as.symbol(cally))
  
  # check that the new object has been created and display a message accordingly
  finalcheck <- isAssigned(datasources, newobj)
  
}
