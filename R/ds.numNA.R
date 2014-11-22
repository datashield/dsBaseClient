#' 
#' @title Gets the number of missing values in a vector
#' @description In DataSHIELD it is not possible to visualize the data. This
#' function helps to know the number of missing values in a vector to eventually use a 
#' vector of equal length (i.e. the count of missing entries) to replace the missing values.
#' @details The vector to check for missing values might be a in a table structure or not. 
#' The number of missing entries are counted and the total for each study is returned.
#' @param x a character, the name of a vector to check for missing entries.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @return for an array, \code{NULL} or a vector of mode \code{integer}
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
#' # Example 1: Get the number of missing values in the variable 'LAB_HDL' 
#' held in the table 'D'. 
#' ds.numNA(x='D$LAB_HDL')
#' 
#' # Example 2: Assign the above variable and check the 
#' number of missing values on the now loose variable 'LAB_HDL'
#' ds.assign(toAssign='D$LAB_HDL', newobj='labhdl')
#' # Example 2: Get the pooled dimension of the assigned datasets
#' ds.numNA(x='labhdl')
#' 
#' # clear the Datashield R sessions and logout
#' datashield.logout(opals)
#' 
#' }
#' 
ds.numNA = function(x=NULL, datasources=NULL) {
  
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
  
  # check if the input object(s) is(are) defined in all the studies
  inputElts <- extract(x)
  if(is.na(inputElts[[1]])){
    defined <- isDefined(datasources, inputElts[[2]])
  }else{
    defined <- isDefined(datasources, inputElts[[1]])
  }

  # call the server side function
  cally <- paste0("numNaDS(", x, ")")
  numNAs <- datashield.aggregate(datasources, as.symbol(cally))
  
  return(numNAs)
}