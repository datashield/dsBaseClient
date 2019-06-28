#' 
#' @title Recodes the levels of a factor vector
#' @description The function replaces the levels of a factor by the specified ones.
#' @details It uses the R function 'levels()' on the client side to alter the current levels.
#' It can for example be used to merge two classes into one, to add a level(s) to a vector 
#' or to rename (i.e. re-label) the levels of a vector.
#' @param x, a character, the name of a factor variable.
#' @param newCategories, a character vector, the new levels. Its length MUST be equal or greater 
#' to the current number of levels.
#' @param newobj, a character, the name of the new factor vector. If no name is specified
#' for the new variable it is named after the input variable with a suffixe '_new'.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' By default an internal function looks for 'opal' objects in the environment and sets this parameter. 
#' @return nothing is returned to the client, the new object is stored on the server side.
#' @author Gaye, A.
#' @export
#' @examples
#' \dontrun{
#' 
#'   # load that contains the login details
#'   data(logindata)
#'
#'   # login and assign all the variables
#'   opals <- datashield.login(logins=logindata,assign=TRUE)
#' 
#'   # let s first check the levels in the categorical variable 'PM_BMI_CATEGORICAL'
#'   ds.levels(x='D$PM_BMI_CATEGORICAL')
#' 
#'   # Example1: merge the levels '2' and '3' to obtain only two levels (i.e. '1' and '2')
#'   # this is the same as recoding level '3' as '2' whilst keeping the same labels for the other
#'   # two levels.
#'   ds.recodeLevels(x='D$PM_BMI_CATEGORICAL', newCategories=c('1','2','2'), newobj='BMI_CAT_NEW1')
#'   ds.levels(x='BMI_CAT_NEW1')
#' 
#'   # Example2: add a 4th and empty level to categorical bmi to create a new variable
#'   # we know the current categories are '1', '2' and '3' so we add '4'
#'   ds.recodeLevels(x='D$PM_BMI_CATEGORICAL', newCategories=c('1','2','3','4'), newobj='BMI_CAT_NEW2')
#'   ds.levels(x='BMI_CAT_NEW2')
#' 
#'   # Example3: re-label the levels of the categorical bmi "low", "mid" and "high"
#'   ds.recodeLevels(x='D$PM_BMI_CATEGORICAL', newCategories=c('low','mid','high'),
#'                   newobj='BMI_CAT_NEW3')
#'   ds.levels(x='BMI_CAT_NEW3')
#' 
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(opals)
#' 
#' }
#'
ds.recodeLevels <- function(x=NULL, newCategories=NULL, newobj=NULL, datasources=NULL){
  
  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }
  
  if(is.null(x)){
    message(" ALERT!")
    message(" Please provide a valid numeric of character vector")
    stop(" End of process!", call.=FALSE)
  }
  
  if(is.null(newCategories)){
    message(" ALERT!")
    message(" Please specify the new categories to recode to")
    stop(" End of process!", call.=FALSE)
  }
  
  # check if the input object(s) is(are) defined in all the studies
  defined <- isDefined(datasources,x)
  
  # call the internal function that checks the input object is of the same class in all studies.
  typ <- checkClass(datasources, x)
  
  # if input vector is not a factor stop
  if(typ != 'factor'){
    stop("The input vector must be a factor!", call.=FALSE)
  }
  
  # get the current number of levels
  cally <- paste0("levels(", x, ")")
  xx <- opal::datashield.aggregate(datasources, as.symbol(cally))
  if(length(unique(unlist(xx))) > length(newCategories)){
    stop("The number of levels you specified is smaller than the levels of the input vector!", call.=FALSE)
  }
  
  # the input variable might be given as column table (i.e. D$x)
  # or just as a vector not attached to a table (i.e. x)
  # we have to make sure the function deals with each case
  xnames <- extract(x)
  varname <- xnames[length(xnames)]
  
  # if no name was provided for the new variable give it a default name
  if(is.null(newobj)){
    newobj <- paste(varname, "_new", sep="")
  }

  # get the names and number of the studies/datasources
  stdnames <- names(datasources)
  numstudies <- length(stdnames)
  
  # do the business
  cally <- paste0("recodeLevelsDS(", x, ", c(","'",paste(newCategories,collapse="','"),"')",")")
  opal::datashield.assign(datasources, newobj, as.symbol(cally))
  
  # check that the new object has been created and display a message accordingly
  finalcheck <- isAssigned(datasources, newobj)
}
