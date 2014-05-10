#' 
#' @title Ensures factors have same classes across studies
#' @details This function checks if a factor variable has the same length accross
#' the collaborating studies; if not it adds the missing level(s) to make up
#' for the missing class(es). This is necessary for certain function like \code{ds.glm} which
#' require the same classes across the studies to combine.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources. 
#' @param xvect a factor variable.
#' @param newvarname name of the variable to assigned the created factor to.
#' @return a list of \code{factor} vectors or one factor vector.
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
#' # check the number of levels for the variable PM_BMI_CATEGORICAL in each study
#' # levels of PM_BMI_CATEGORICAL in the 3 studies
#' datashield.aggregate(opals[1], quote(levels(D$PM_BMI_CATEGORICAL)))
#' datashield.aggregate(opals[2], quote(levels(D$PM_BMI_CATEGORICAL)))
#' datashield.aggregate(opals[3], quote(levels(D$PM_BMI_CATEGORICAL)))
#'
#' #create a factor vector of the variable 'PM_BMI_CATEGORICAL', one for each study 
#' ds.amendLevels(opals, xvect=quote(D$PM_BMI_CATEGORICAL))
#'
#  # Now let us check the levels after the correction - by default
#' # the name of the newly created variable is the previous one with the suffixe '_f'
#' datashield.aggregate(opals[1], quote(levels(PM_BMI_CATEGORICAL_f)))
#' datashield.aggregate(opals[2], quote(levels(PM_BMI_CATEGORICAL_f)))
#' datashield.aggregate(opals[3], quote(levels(PM_BMI_CATEGORICAL_f)))
#' }
#'
ds.amendLevels <- function(datasources=NULL, xvect=NULL, newvarname=NULL){
  
  if(is.null(datasources)){
    message(" ALERT!")
    message(" No valid opal object(s) provided.")
    message(" Make sure you are logged in to valid opal server(s).")
    stop(" End of process!", call.=FALSE)
  }
  
  if(is.null(xvect)){
    message(" ALERT!")
    message(" Please provide a valid numeric of character vector")
    stop(" End of process!", call.=FALSE)
  }
  
  # the input variable might be given as column table (i.e. D$xvect)
  # or just as a vector not attached to a table (i.e. xvect)
  # we have to make sure the function deals with each case
  inputterms <- unlist(strsplit(deparse(xvect), "\\$", perl=TRUE))
  if(length(inputterms) > 1){
    var <-  strsplit(deparse(xvect), "\\$", perl=TRUE)[[1]][2]
  }else{
    var <- deparse(xvect)
  }
  
  # if no name was provided for the new variable give it a default name
  if(is.null(newvarname)){
    newvarname <- paste(var, "_f", sep="")
  }
  
  # call the function that checks the variables are available and not empty
  vars2check <- list(xvect)
  datasources <- ds.checkvar(datasources, vars2check)
  
  # get the names and number of the studies/datasources
  stdnames <- names(datasources)
  numstudies <- length(stdnames)
  
  # get the classes in each study for the input variable
  classes <- list()
  # check if the input variable is a factor, if not stop and display a message
  for(i in 1:numstudies){
    cally <- call("is.factor", xvect) 
    if(!(datashield.aggregate(datasources[i], cally)[[1]])){
      stop("The variable ", var, " in ", stdnames[i], " is not a factor!" )
    }
    cally <- call("levels", xvect) 
    ll <- datashield.aggregate(datasources[i], cally)
    classes <- append(classes, ll)
  }
  
  # if the studies have different levels insert an 'empty' class where required
  all.levels <- unique(unlist(classes))
  # check if there is any study that has different levels
  check <- FALSE
  indx1 <- c()
  for(i in 1:numstudies){
   xx <- which(!(all.levels %in% classes[[i]]))
   if(length(xx) > 0){ 
     indx1 <- append(indx1, i)
     check = TRUE
   }
  }
  if(check){
    message("The studies do not have the same levels for ", var)
    cally <- call("createfactor.ds", xvect, as.list(all.levels))
    datashield.assign(datasources, newvarname, cally)
    message("Adjusted! Now all studies have the same levels.")
    message("The name of the new variable is: ", newvarname)
  }else{
    message("All the studies have the same levels for the variable ", var)
  }
}
