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
#' # create a login table for the test data that has the features we want to test
#' server <- c("study1", "study2", "study3")
#' url <- c("http://54.242.140.255","http://54.242.46.59", "http://23.22.215.42")
#' user <- c("administrator", "administrator", "administrator")
#' password <- c("password", "password", "password")
#' table <- c("Test.CNSIM", "Test.CNSIM", "Test.CNSIM")
#' logindata <- data.frame(server,url,user,password,table)

#' # login and assign specific variable(s)
#' myvar <- list("PM_BMI_CATEGORICAL")
#' opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

#' # check the number of levels for the variable PM_BMI_CATEGORICAL in each study
#' # levels of PM_BMI_CATEGORICAL in the 3 studies
#' datashield.aggregate(opals[1], quote(levels(D$PM_BMI_CATEGORICAL)))
#' datashield.aggregate(opals[2], quote(levels(D$PM_BMI_CATEGORICAL)))
#' datashield.aggregate(opals[3], quote(levels(D$PM_BMI_CATEGORICAL)))
#'
#' #create a factor vector of the variable 'PM_BMI_CATEGORICAL', one for each study 
#' ds.createfactor(opals, xvect=quote(D$PM_BMI_CATEGORICAL))
#'
#  # Now let us check the levels after the correction - by default
#' # the name of the newly created variable is the previous one with the suffixe '_f'
#' datashield.aggregate(opals[1], quote(levels(PM_BMI_CATEGORICAL_f)))
#' datashield.aggregate(opals[2], quote(levels(PM_BMI_CATEGORICAL_f)))
#' datashield.aggregate(opals[3], quote(levels(PM_BMI_CATEGORICAL_f)))
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
    if(!(datashield.aggregate(opals[i], cally)[[1]])){
      stop("\n\nThe variable ", var, " in ", stdnames[i], " is not a factor!\n\n" )
    }
    cally <- call("levels", xvect) 
    ll <- datashield.aggregate(opals[i], cally)
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
    cat("\n\nThe studies do not have the same levels for ", var,":\n")
    cally <- call("createfactor.ds", xvect, as.list(all.levels))
    datashield.assign(datasources, newvarname, cally)
    cat("Adjusted! Now all studies have the same levels.\n\n")
    cat("The name of the new variable is: ", newvarname, "\n\n")
  }else{
    cat("\nAll the studies have the same levels for the variable ", var, "\n\n")
  }
}
