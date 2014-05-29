#' 
#' @title Turns a vector into numerical type
#' @description This function is similar to R function \code{as.numeric}.
#' @details See details of the R function 'as.numeric'.
#' @param xvect a character, the name of the input vector.
#' @param newobj the name of the new variable. If this argument is set to NULL, the name of the new 
#' variable is the name of the input variable with the suffixe '_num'.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' By default an internal function looks for 'opal' objects in the environment and sets this parameter. 
#' @return nothing is returned to the client, the new object is stored on the server side.
#' @author Gaye, A.
#' @export
#' @examples {
#' 
#' # load that contains the login details
#' data(logindata)
#' 
#' # login and assign specific variable(s)
#' myvar <- list("GENDER")
#' opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#' # turn the factor variable 'GENDER' into a character vector
#' ds.asCharacter(xvect='D$GENDER', newobj="gender_ch")
#' 
#' # now turn the newly created vector 'gender_ch' into a numeric
#' ds.asNumeric(xvect='gender_ch', newobj="gender_nm")
#' }
#' 
ds.asNumeric = function(xvect=NULL, newobj=NULL, datasources=NULL){
  
  # if no opal login details were provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    findLogin <- dsbaseclient:::.getOpals()
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
  
  if(is.null(xvect)){
    message("\n\n ALERT!\n")
    message(" Please provide a valid numeric vector\n")
    stop(" End of process!\n\n", call.=FALSE)
  }
  
  # check if the input object(s) is(are) defined in all the studies
  defined <- isDefined(datasources,xvect)
  
  # call the internal function that checks the input object is of the same class in all studies.
  typ <- dsbaseclient:::.checkClass(datasources, xvect)
  
  # the input variable might be given as column table (i.e. D$xvect)
  # or just as a vector not attached to a table (i.e. xvect)
  # we have to make sure the function deals with each case
  inputterms <- unlist(strsplit(xvect, "\\$", perl=TRUE))
  if(length(inputterms) > 1){
    varname <- strsplit(xvect, "\\$", perl=TRUE)[[1]][2]
  }else{
    varname <- xvect
  }

  # create a name by default if user did not provide a name for the new variable
  if(is.null(newobj)){
    newobj <- paste0(varname, "_num")
  }
  
  # call the server side function that does the job
  cally <- paste0('as.numeric(', xvect, ')' )
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
