#' 
#' @title Replaces the missing values in a vector
#' @description This function identifies missing values and replaces them by a value or 
#' values specified by the analyst.
#' @details This function is used when the analyst prefer or requires complete vectors. 
#' It is then possible the specify one value for each missing value by first returning
#' the number of missing values using the function \code{ds.numNA} but in most cases
#' it might be more sensible to replace all missing values by one specific value e.g. 
#' replace all missing values in a vector by the mean or median value. Once the missing
#' values have been replaced a new vector is created. If the vector is within a table 
#' structure such as a data frame a new data frame is generated with the new vector.
#' @param x a character, the name of the vector to process.
#' @param forNA a list which contains the replacement value(s), a vector one or more values 
#' for each study. The length of the list must be equal to the number of servers the analyst 
#' is connected to. 
#' @param newobj a character, the name of the new vector or table structure in which missing 
#' values have been replaced. If no name is specified the default name is the name of the original 
#' vector or table structure followed by the suffix '_noMissings' e.g. 'LAB_HDL_noNA' or 
#' 'D_LAB_HDL_noNA' if 'LAB_HDL' is in table 'D'.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @return a new vector or table structure with the same class is stored on the server site.
#' @author Gaye, A.
#' @export
#' @examples {
#' 
#' # load that contains the login details
#' data(logindata)
#' 
#' # login and assign all the stored variables.
#' opals <- datashield.login(logins=logindata,assign=TRUE)
#' 
#' # Replace missing values in variable 'LAB_HDL' by the mean value in each study
#' # first let us get the mean value for 'LAB_HDL' in each study
#' ds.mean(x='D$LAB_HDL', type='split')
#' # now do the repalcement
#' ds.repalceNA(x='D$LAB_HDL', forNA=list(1.569416, 1.556648), newobj='D_HDL_noNA')
#' 
#' # clear the Datashield R sessions and logout
#' datashield.logout(opals)
#' 
#' }
#' 
ds.replaceNA = function(x=NULL, forNA=NULL, newobj=NULL, datasources=NULL) {
  
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
    stop("Please provide the name of a vector!", call.=FALSE)
  }
  
  # check if replacement values have been provided
  if(is.null(forNA)){
    stop("Please provide a list of replacement values!", call.=FALSE)
  }else{
    if(length(forNA != length(datasources))){
      stop("'forNA' must be of the same length as the number of datasources/studies!", call.=FALSE)
    }
  }
  
  # check if the input object(s) is(are) defined in all the studies
  inputElts <- extract(x)
  if(is.na(inputElts[[1]])){
    defined <- isDefined(datasources, inputElts[[2]])
  }else{
    defined <- isDefined(datasources, inputElts[[1]])
  }
  
  for(i in 1:length(datasources)){
    # get the number of missing values for each study and if the number of 
    # replacement values is not 1 and is greater or smaller than the actual 
    # number of missing values stop the process and tell the analyst
    cally <- paste0("numNaDS(", x, ")")
    numNAs <-unlist(datashield.aggregate(datasources[i], as.symbol(cally)))  
    if(numNAs != 1 & numNAs != forNA[[i]]){
      message("The number of replacement values must be of length 1 or of the same length as the number of missing values.")
      stop(paste0("This is not the case in ", names(datasources)[i]), call.=FALSE)
    }
  }
  
  if(is.null(newobj)){
    if(!(is.na(inputElts[[1]]))){
      newobj <- paste0(inputElts[[1]],"_",inputElts[[2]],"_noNA")
    }else{
      newobj <- paste0(inputElts[[2]],"_noNA")
    }
  }
  
  # call the server side function and doo the replacement for each server
  for(i in 1:length(datasources)){
    message(paste0("--Processing ", names(datasources)[i], "..."))
    cally <- call("replaceNaDS", as.symbol(x), forNA[[i]], newobj)
    numNAs <- datashield.aggregate(datasources, as.symbol(cally))
    
    # check that the new object has been created and display a message accordingly
    finalcheck <- isAssigned(datasources, newobj) 
  }
  
}