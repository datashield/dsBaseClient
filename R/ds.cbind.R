#' 
#' @title Combines objects by columns
#' @description this is similar to the R base function 'cbind'
#' @details the function combines vectors or vectors and matrices or dataframes by columns.
#' But unlike the R base function 'cbind' the output is a dataframe.
#' @param x a vector which holds the names of the objects to combine
#' @param newobj the name of the output object. If this argument is set to \code{NULL}, 
#' the name of the new object is 'newDataframe'.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @return nothing is returned to the client, the new object is stored on the server side.
#' @author Gaye, A.
#' @export
#' @examples {
#' 
#' # load the file that contains the login details
#' data(logindata)
#' 
#' # login and assign specific variables(s)
#' # (by default the assigned dataset is a dataframe named 'D')
#' myvar <- list('LAB_TSC', 'LAB_HDL')
#' opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#' # generate a new dataframe by combining the log values of 'LAB_TSC' and 'LAB_HDL', by columns
#' ds.assign(newobj='labtsc', toAssign='log(D$LAB_TSC)')
#' ds.assign(newobj='labhdl', toAssign='log(D$LAB_HDL)')
#' myobjects <- c('labtsc', 'labhdl')
#' ds.cbind(x=myobjects)
#' 
#' # clear the Datashield R sessions and logout
#' datashield.logout(opals)
#' 
#' }
#' 
ds.cbind = function(x=NULL, newobj=NULL, datasources=NULL){

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
    stop("x=NULL. Please provide the names of the objects to combine!", call.=FALSE)
  }
  
  if(length(x) < 2){
    stop("You must provide the names of at least two objects!\n", call.=FALSE)
  }
  
  # the input variable might be given as column table (i.e. D$object)
  # or just as a vector not attached to a table (i.e. object)
  # we have to make sure the function deals with each case
  xnames <- extract(x)
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
  for(i in 1:length(x)){
    typ <- checkClass(datasources, x[i])
  }
    
  if(is.null(newobj)){
    newobj <- 'newDataframe'
  }
  
  # call the server side function
  cally <-  paste0("cbindDS(list(",paste(x,collapse=","),")", 
                   ",list(","'",paste(varnames,collapse="','"),"'","))")
  datashield.assign(datasources, newobj, as.symbol(cally))
  
  # check that the new object has been created and display a message accordingly
  finalcheck <- isAssigned(datasources, newobj)
  
}