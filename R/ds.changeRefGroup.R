#' 
#' @title Changes the reference level of a factor
#' @description This function is similar to R function \code{relevel}.
#' @details In addition to what the R function does, this function
#' allows for the user to re-order the vector, putting the reference
#' group first. If the user chooses the re-order a warning is issued 
#' as this can introduce a mismatch of values if the vector is put back
#' into a table that is not reordered in the same way. Such mismatch
#' can render the results of operations on that table invalid.
#' @param xvect a character, the name of a vector of type factor.
#' @param ref the reference level
#' @param newobj the name of the new variable. If this argument is set to NULL, 
#' the name of the new variable is the name of the input variable with the 
#' suffixe '_newref'.
#' @param reorderByRef a boolean that tells whether or not the new vector 
#' should be ordered by the reference group (i.e. putting the reference group first).
#' The default is to not re-order for the reasons explained in the 'details' section.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @return nothing is returned to the client, the new object is stored on the server side.
#' @author Isaeva, I.; Gaye, A.
#' @export
#' @examples {
#' 
#' # load that contains the login details
#' data(logindata)
#' 
#' # login and assign all the stored variables
#' # (by default the assigned dataset is a dataframe named 'D')
#' opals <- datashield.login(logins=logindata,assign=TRUE)
#' 
#' # Example 1: rename the categories and change the reference with re-ordering
#' # print out the levels of the initial vector
#' ds.levels(xvect='D$PM_BMI_CATEGORICAL')
#' 
#' # define a vector with the new levels and recode the initial levels
#' newNames <- c('normal', 'overweight', 'obesity')
#' ds.recodeLevels(xvect='D$PM_BMI_CATEGORICAL', newCategories=newNames, newobj='bmi_new')
#' 
#' # print out the levels of the new vector
#' ds.levels(xvect='bmi_new')
#' 
#' # by default the reference is the first level in the vector of levels (here 'normal')
#' # now change the set the reference to 'obesity' without changing the order (default)
#' ds.changeRefGroup(xvect='bmi_new', ref='obesity', newobj='bmi_ob')
#' 
#' # print out the levels; the first listed level (i.e. the reference) is now 'obesity'
#' ds.levels(xvect='bmi_ob')
#' 
#' # Example 2: change the reference and re-order by the refence level
#' # If re-ordering is sought, the action is completed but a warning is issued.
#' ds.recodeLevels(xvect='D$PM_BMI_CATEGORICAL', newCategories=newNames, newobj='bmi_new')
#' ds.changeRefGroup(xvect='bmi_new', ref='obesity', newobj='bmi_ob', reorderByRef=TRUE)
#' 
#' }
#' 
ds.changeRefGroup = function(xvect=NULL, ref=NULL, newobj=NULL, reorderByRef=FALSE, datasources=NULL){
  
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
  
  if(is.null(xvect)){
    message("\n\n ALERT!\n")
    message(" Please provide a valid factor vector\n")
    stop(" End of process!\n\n", call.=FALSE)
  }
  
  if(is.null(ref)){
    stop(" You must indicate a reference level by setting the parameter 'ref'.", call.=FALSE)
  }
  
  # check if the input object(s) is(are) defined in all the studies
  defined <- isDefined(datasources, xvect)
  
  # call the internal function that checks the input object is of the same class in all studies.
  typ <- checkClass(datasources, xvect)
  
  # if input vector is not a factor stop
  if(typ != 'factor'){
    stop("The input vector must be a factor!", call.=FALSE)
  }
  
  if(reorderByRef){
    warning("'reorderByRef' is set to TRUE. Please read the documentation for possible consequences!", call.=FALSE)
  }
  
  # the input variable might be given as column table (i.e. D$xvect)
  # or just as a vector not attached to a table (i.e. xvect)
  # we have to make sure the function deals with each case
  xnames <- extract(xvect)
  varname <- xnames[length(xnames)]
  
  # create a name by default if user did not provide a name for the new variable
  if(is.null(newobj)){
    newobj <- paste0(varname, "_newref")
  }
  
  # call the server side function that will recode the levels
  cally <- paste0('changeRefGroupDS(', xvect, ",'", ref, "',", reorderByRef,")")
  datashield.assign(datasources, newobj, as.symbol(cally))
  
  # check that the new object has been created and display a message accordingly
  finalcheck <- isAssigned(datasources, newobj)
  
}
