#' 
#' @title Tests for correlation between paired samples
#' @description This is similar to the R base function 'cor.test'.
#' @details Runs a two sided pearson test with a 0.95 confidence level.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @param x a character, the name of a numerical vector
#' @param y a character, the name of a numerical vector
#' @return a list containing the results of the test
#' @author Gaye, A.; Burton, P.
#' @export
#' @examples
#' \dontrun{
#' 
#'   # load that contains the login details
#'   data(logindata)
#' 
#'   # login and assign specific variable(s)
#'   # (by default the assigned dataset is a dataframe named 'D')
#'   myvar <- list('LAB_TSC', 'LAB_HDL')
#'   opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#'   # test for correlation between the variables 'LAB_TSC' and 'LAB_HDL'
#'   ds.corTest(x='D$LAB_TSC', y='D$LAB_HDL')
#' 
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(opals)
#' 
#' }
#' 
ds.corTest = function(x=NULL, y=NULL, datasources=NULL){
  
  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }
  
  if(is.null(x)){
    stop("x=NULL. Please provide the names of the 1st vector!", call.=FALSE)
  }
  if(is.null(y)){
    stop("y=NULL. Please provide the names of the 2nd numeric vector!", call.=FALSE)
  }
  
  # the input variable might be given as column table (i.e. D$object)
  # or just as a vector not attached to a table (i.e. object)
  # we have to make sure the function deals with each case
  objects <- c(x,y)
  xnames <- extract(objects)
  varnames <- xnames$elements
  obj2lookfor <- xnames$holders
  
  # check if the input object(s) is(are) defined in all the studies
  for(i in 1:length(varnames)){
    if(is.na(obj2lookfor[i])){
      defined <- isDefined(datasources, varnames[i])
    }else{
      defined <- isDefined(datasources, obj2lookfor[i])
    }
  }
  
  # call the internal function that checks the input object(s) is(are) of the same class in all studies.
  for(i in 1:length(objects)){
    typ <- checkClass(datasources, objects[i])
  }
  
  # call the server side function
  cally <- paste0("cor.test(", x, ",", y, ")")
  res.local <- opal::datashield.aggregate(datasources, as.symbol(cally))
  
  return(res.local)
    
}
