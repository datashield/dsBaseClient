#' 
#' @title Attempts to turn its argument into a matrix
#' @description This function is similar to R function \code{as.matrix}. 
#' @details Unlike the R function 'as.matrix' the output matrix is generated only if the input matrix
#' is valid (i.e. has a number of rows greater than the minimal number of observations allowed).
#' @param x a character, the name of the an object to conver to a matrix.
#' @param newobj the name of the new vector.If this argument is set to \code{NULL}, the name of the new 
#' variable is the name of the input variable with the suffixe '_matrix'
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' By default an internal function looks for 'opal' objects in the environment and sets this parameter. 
#' @return nothing is returned to the client, the new object is stored on the server side.
#' @author Gaye, A.; Isaeva, J.
#' @export
#' @examples {
#' 
#' # load that contains the login details
#' data(logindata)
#' 
#' # login and assign specific variable(s)
#' opals <- datashield.login(logins=logindata,assign=TRUE)
#' 
#' # turn the data frame 'D' (default name of the dataframe assigned above) into a matrix
#' ds.asMatrix(datasources=opals, x='D')
#' }
#' 
ds.asMatrix = function(x=NULL, newobj=NULL, datasources=NULL){
  
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
        stop(" Please set the parameter 'datasources' to the list you want to use. ", call.=FALSE)
      }
    }
  }
  
  if(is.null(x)){
    message("\n ALERT!\n")
    message(" Please provide a valid input.")
    stop(" End of process!\n", call.=FALSE)
  }
  
  # check if the input object(s) is(are) defined in all the studies
  defined <- isDefined(datasources, x)
  
  # call the internal function that checks the input object is of the same class in all studies.
  typ <- checkClass(datasources, x)
  
  # Only a dataframe or a matrice can be turned into a list
  if(typ != 'data.frame' & typ != 'factor' & typ != 'character' & typ != 'numeric' & typ != 'integer'  & typ != 'logical'){
    message(paste0("Your object is of type ", typ, "!"))
    stop(" Only objects of type 'data.frame', 'numeric', 'integer', 'character', 'factor' and 'logical' are allowed.", call.=FALSE)
  }
  
  # the input variable might be given as column table (i.e. D$x)
  # or just as a vector not attached to a table (i.e. x)
  # we have to make sure the function deals with each case
  xnames <- extract(x)
  varname <- xnames[length(xnames)]
  
  # create a name by default if user did not provide a name for the new variable
  if(is.null(newobj)){
    newobj <- paste0(varname, "_matrix")
  }
  
  # call the server side function that does the job
  cally <- paste0("asMatrixDS(", x, ")")
  datashield.assign(datasources, newobj, as.symbol(cally))
  
  # check that the new object has been created and display a message accordingly
  finalcheck <- isAssigned(datasources, newobj)
  
}