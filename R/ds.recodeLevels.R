#' 
#' @title Recodes the levels of a factor vector
#' @description The function replaces the levels of a factor by the specified ones.
#' @details It uses the R function 'levels()' on the client side to alter the current levels.
#' It can for example be used to merge two classes into one, to add a level(s) to a vector 
#' or to rename (i.e. re-label) the levels of a vector.
#' @param xvect, a character, the name of a factor variable.
#' @param categories, a character vector, the new levels. Its length MUST be equal or greater 
#' to the current number of levels.
#' @param newobj, a character, the name of the new factor vector. If no name is specified
#' for the new variable it is named after the input variable with a suffixe '_new'.
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
#' myvar <- list("PM_BMI_CATEGORICAL")
#' opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#' # let s first check the levels in the categorical variable 'PM_BMI_CATEGORICAL'
#' ds.levels(xvect='D$PM_BMI_CATEGORICAL')
#' 
#' # Example1: merge the levels '2' and '3' to obtain only two levels (i.e. '1' and '2')
#' # this is the same as recording level '3' as '2' whilst keeping the same labels for the other two levels.
#' ds.recodeLevels(xvect='D$PM_BMI_CATEGORICAL', categories=c('1','2','2'))
#' 
#' # Example2: add a 4th and empty level to categorical bmi to create a new variable
#' # we know the current categories are '1', '2' and '3' so we add '4'
#' ds.recodeLevels(xvect='D$PM_BMI_CATEGORICAL', categories=c('1','2','3','4'))
#' 
#' # Example3: re-label the levels of the categorical bmi "low", "mid" and "high"
#' ds.recodeLevels(xvect='D$PM_BMI_CATEGORICAL', categories=c('low','mid','high'))
#'
#' }
#'
ds.recodeLevels <- function(xvect=NULL, categories=NULL, newobj=NULL, datasources=NULL){
  
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
    message(" ALERT!")
    message(" Please provide a valid numeric of character vector")
    stop(" End of process!", call.=FALSE)
  }
  
  if(is.null(categories)){
    message(" ALERT!")
    message(" Please specify the categories to recode to")
    stop(" End of process!", call.=FALSE)
  }
  
  # call the internal function that checks the input object is of the same class in all studies.
  typ <- checkClass(datasources, xvect)
  
  # if input vector is not a factor stop
  if(typ != 'factor'){
    stop("The input vector must be a factor!", call.=FALSE)
  }
  
  # get the current number of levels
  cally <- paste0("levels(", xvect, ")")
  xx <- datashield.aggregate(datasources, as.symbol(cally))
  if(length(unique(unlist(xx))) > length(categories)){
    stop("The number of levels you specified is smaller than the levels of the input vector!", call.=FALSE)
  }
  
  # the input variable might be given as column table (i.e. D$xvect)
  # or just as a vector not attached to a table (i.e. xvect)
  # we have to make sure the function deals with each case
  inputterms <- unlist(strsplit((xvect), "\\$", perl=TRUE))
  if(length(inputterms) > 1){
    var <-  strsplit(xvect, "\\$", perl=TRUE)[[1]][2]
  }else{
    var <- xvect
  }
  # if no name was provided for the new variable give it a default name
  if(is.null(newobj)){
    newobj <- paste(var, "_new", sep="")
  }

  # get the names and number of the studies/datasources
  stdnames <- names(datasources)
  numstudies <- length(stdnames)
  
  # do the business
  cally <- paste0("recodeLevelsDS(", xvect, ", c(","'",paste(categories,collapse="','"),"')",")")
  datashield.assign(datasources, newobj, as.symbol(cally))
  
  # check that the new object has been created and display a message accordingly
  cally <- paste0('exists(', newobj, ')')
  qc <- datashield.aggregate(datasources, cally)
  indx <- as.numeric(which(qc==TRUE))
  
  if(length(indx) > 0 & length(indx) < length(datasources)){
    stop("The output object, '", newobj, "', was generated only for ", names(datasources)[indx], "!", call.=FALSE)
  }
  if(length(indx) == 0){
    stop("The output object has not been generated for any of the studies!", call.=FALSE)
  }

}
