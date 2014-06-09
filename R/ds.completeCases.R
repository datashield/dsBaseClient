#' 
#' @title Find complete cases
#' @description This function is similar to R function \code{complete.cases}. 
#' @details a logical vector indicating which cases are complete(i.e. have no missing values)
#' is generated and stored on the server side. That object can then be tabulated using the 
#' function 'ds.table1d' to find the extent of completeness.
#' @param x a character, the name of a vector, dataframe or matrix.
#' @param newobj the name of the new vector.If this argument is set to \code{NULL}, the name of the new 
#' variable is the name of the input variable with the suffixe '_complete'.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @return nothing is returned to the client, the new object is stored on the server side.
#' @author Gaye, A.; Isaeva, J.
#' @export
#' @examples {
#' 
#' # load that contains the login details
#' data(logindata)
#' 
#' # login and assign all the stored variables 
#' # (by default the assigned dataset is a datframe named 'D')
#' opals <- datashield.login(logins=logindata,assign=TRUE)
#' 
#' # Example 1: check completes cases on the dataframe 'D' 
#' ds.completeCases(x='D')
#' # now tabulate the vector of comple cases 
#' # remember default name is name of variable with suffix '_complete'
#' ds.table1d(xvect='D_complete')
#' 
#' # Example 2: check complte cases for the variable 'PM_BMI_CONTINUOUS'
#' ds.completeCases(x='D$PM_BMI_CONTINUOUS')
#' # now tabulate the vector of comple cases 
#' ds.table1d(xvect='PM_BMI_CONTINUOUS_complete')
#' 
#' }
#' 
ds.completeCases = function(x=NULL, newobj=NULL, datasources=NULL){
  
  # if no opal login details were provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    findLogin <- getOpals()
    if(findLogin$flag == 1){
      datasources <- findLogin$opals
    }else{
      if(findLogin$flag == 0){
        stop(" Are yout logged in to any server? Please provide a valid opal login object! ", call.=FALSE)
      }else{
        message(paste0("More than one list of opal login object were found: '", paste(findLogin$opals,collapse="', '"), "'!"))
        userInput <- readline("Please enter the name of the login object you want to use: ")
        datasources <- eval(parse(text=userInput))
        if(class(datasources[[1]]) != 'opal'){
          stop("End of process: you failed to enter a valid login object", call.=FALSE)
        }
      }
    }
  }
  
  if(is.null(x)){
    stop("Please provide the name of a vector, data.frame or matrix!", call.=FALSE)
  }
  
  # check if the input object(s) is(are) defined in all the studies
  defined <- isDefined(datasources, x)
  
  # call the internal function that checks the input object is of the same class in all studies.
  typ <- checkClass(datasources, x)
  
  # Only a dataframe or a matrice can be turned into a list
  if(typ != 'data.frame' & typ != 'factor' & typ != 'character' & typ != 'numeric' & typ != 'integer' & typ != 'matrix' & typ != 'logical'){
    message(paste0("Your object is of type ", typ, "!"))
    stop(" Input must be of type 'data.frame', 'numeric', 'integer', 'character', 'factor', 'matrix' or 'logical'.", call.=FALSE)
  }
  
  # the input variable might be given as column table (i.e. D$x)
  # or just as a vector not attached to a table (i.e. x)
  # we have to make sure the function deals with each case
  xnames <- extract(x)
  varname <- xnames[length(xnames)]
  
  # create a name by default if user did not provide a name for the new variable
  if(is.null(newobj)){
    newobj <- paste0(varname, "_complete")
  }
  
  # call the server side function that does the job
  cally <- paste0('complete.cases(', x, ')')
  datashield.assign(datasources, newobj, as.symbol(cally))
  
  # check that the new object has been created and display a message accordingly
  finalcheck <- isAssigned(datasources, newobj)
  
}