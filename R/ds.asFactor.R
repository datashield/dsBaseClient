#' 
#' @title Turns a numeric vector into factor type
#' @description This function is similar to R function \code{as.factor} but it does not allow users
#' to create factors where a categorie has less than two observations.
#' @details Unlike the R function 'as.factor' vectors where some levels have less than 2 observations
#' are not allow, an empty vector (i.e. all values within it are set to NA) is then generated.
#' @param xvect a character, the name of numeric, integer or character vector
#' @param newobj the name of the new vector.If this argument is set to NULL, the name of the new 
#' variable is the name of the input variable with the suffixe '_fact'.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' By default an internal function looks for 'opal' objects in the environment and sets this parameter. 
#' @return nothing is returned to the client, the new object is stored on the server side.
#' @author Gaye, A.; Burton, P.
#' @export
#' @examples {
#' 
#' # load that contains the login details
#' data(logindata)
#' 
#' # login and assign specific variable(s)
#' myvar <- list('GENDER', 'LAB_HDL')
#' opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#' # turn the factor variable 'GENDER' into numeric and then back into factor
#' ds.asNumeric(xvect='D$GENDER', newobj='gender_num')
#' ds.asFactor(xvect='gender_num', newobj='gender_fact')
#' 
#' # Now try to turn into a factor a numeric variable where some levels have less
#' # than 2 observations (as you would expect for a continuous variable)
#' # this will generate an 'empty' vector (i.e. all values within it are set to NA).
#' ds.asFactor(xvect='D$LAB_HDL', newobj='lab.hdl.fact')
#' # let us check the levels of the new vector
#' ds.levels('lab.hdl.fact')
#' 
#' }
#' 
ds.asFactor = function(xvect=NULL, newobj=NULL, datasources=NULL){
  
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
  
  if(is.null(xvect)){
    message("\n ALERT!\n")
    message(" Please provide a valid numeric vector.")
    stop(" End of process!\n", call.=FALSE)
  }
  
  # check if the input object(s) is(are) defined in all the studies
  defined <- isDefined(datasources, xvect)
  
  # call the internal function that checks the input object is of the same class in all studies.
  typ <- checkClass(datasources, xvect)
  
  # the input variable might be given as column table (i.e. D$xvect)
  # or just as a vector not attached to a table (i.e. xvect)
  # we have to make sure the function deals with each case
  xnames <- extract(xvect)
  varname <- xnames[length(xnames)]
  
  # create a name by default if user did not provide a name for the new variable
  if(is.null(newobj)){
    newobj <- paste0(varname, "_char")
  }
  
  # call the server side function that does the job; 
  # if the input vector is of type 'numeric' or integer turn it first into character
  # as turning a numeric directly into a factor can produce weird results.
  if(typ == 'numeric' | typ == 'integer' | typ == 'logical'){
    cally <- paste0('as.character(', xvect, ')' )
    datashield.assign(datasources, 'tempvect', as.symbol(cally))
    cally <- 'asFactorDS(tempvect)'
    datashield.assign(datasources, newobj, as.symbol(cally))
  }else{
    cally <- paste0('asFactorDS(', xvect, ')' )
    datashield.assign(datasources, newobj, as.symbol(cally))
  }
  
  # check that the new object has been created and display a message accordingly
  finalcheck <- isAssigned(datasources, newobj)
  
}
