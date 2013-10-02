#' 
#' @title create and combines factor vectors.
#' @description This function calls a server function that generates \code{factor} vectors.
#' If the generated factor is not valid (i.e. if any of the categories has a count > 0 and < 5)
#' its values are replaced by missing values.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources. 
#' @param xvect a numeric of character vector.
#' @param newvarname name of the variable to assigned the created factor to.
#' @return a list of \code{factor} vectors or one factor vector.
#' @author Gaye, A.
#' @export
#' @examples {
#' 
#' # load the login data
#' data(logindata)
#' 
#' # login and assign specific variable(s)
#' myvar <- list("GENDER")
#' opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#' # create a factor vector of the variable 'GENDER', one for each study 
#' ds.createfactor(datasources=opals, "sex", xvect=quote(D$GENDER))
#' }
#'
ds.createfactor <- function(datasources=NULL, xvect=NULL, newvarname=NULL){
  
  if(is.null(datasources)){
    cat("\n\n ALERT!\n")
    cat(" No valid opal object(s) provided.\n")
    cat(" Make sure you are logged in to valid opal server(s).\n")
    stop(" End of process!\n\n", call.=FALSE)
  }
  
  if(is.null(xvect)){
    cat("\n\n ALERT!\n")
    cat(" Please provide a valid numeric of character vector\n")
    stop(" End of process!\n\n", call.=FALSE)
  }
  
  # get the names of the studies/datasources and the name of the variable   
  stdnames <- names(datasources)
  var <-  strsplit(deparse(xvect), "\\$", perl=TRUE)[[1]][2]
  
  # call the function that checks the variables are available and not empty
  vars2check <- list(xvect)
  datasources <- ds.checkvar(datasources, vars2check)
  
  # if no name has been specified for the newly created factor
  # use the name of the input variable
  if(is.null(newvarname)){
    newvarname <- var
  }
    
  # create the factor - one for each study
  cat("\nGenerating factor vectors; non valid vectors will be assigned 'NA' values!\n")
  cally <- call("createfactor.ds", xvect) 
  datashield.assign(datasources, newvarname, cally)

}
