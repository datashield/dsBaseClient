#' 
#' @title Creates data frames
#' @description This is similar to the r function 'data.frame'.
#' @details Takes one or more vectors and generates a data frame structure.
#' # if the sought data frame is not valid (i.e. contains a number of rows less than
#' the minimum allowed number of observation in DataSHIELD), an empty data frame is 
#' created (i.e. a data frame that holds missing values only - NA).
#' @param x a character vector which contains the name(s) of the vector(s) to combine.
#' @param row.names NULL or a character vector specifying the names of the rows.
#' @param check.rows logical, if TRUE then the rows are checked for consistency of length and names.
#' @param check.names logical, logical. If TRUE then the names of the variables in the data frame 
#' are checked to ensure that they are syntactically valid variable names and are not duplicated. 
#' @param stringsAsFactors logical, tells if character vectors should be converted to factors?
#' @param newobj the name of the output object. If this argument is set to \code{NULL}, 
#' the name of the new object is 'dframe'.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @return  nothing is returned to the client, the new object is stored on the server side.
#' @author Gaye, A.; Isaeva, J.
#' @export
#' @examples {
#' 
#' # load the file that contains the login details
#' data(logindata)
#' 
#' # login and assign the required variables to R
#' myvar <- list('LAB_TSC','LAB_HDL')
#' opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#' # create a dataframe that contains the variables the 'LAB_TSC' and 'LAB_HDL'
#' # all the arguments are set to default in this example
#' myvectors <- c('D$LAB_TSC', 'D$LAB_HDL')
#' ds.dataframe(x=myvectors)
#' 
#' }
#' 
ds.dataframe = function(x=NULL,newobj=NULL,row.names=NULL,check.rows=FALSE,check.names=TRUE,stringsAsFactors=TRUE,datasources=NULL){
  
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
    stop("Please provide the name of the list that holds the input vectors!", call.=FALSE)
  }
  
  # the input variable might be given as column table (i.e. D$vector)
  # or just as a vector not attached to a table (i.e. vector)
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
    if(typ != 'factor' & typ != 'character' & typ != 'numeric' & typ != 'integer'  & typ != 'logical'){
      message(paste0(x[i]," is of type ", typ, "!"))
      stop(" Only objects of type 'numeric', 'integer', 'character', 'factor' and 'logical' are allowed.", call.=FALSE)
    }
  }
  
  # create a name by default if user did not provide a name for the new variable
  if(is.null(newobj)){
    newobj <- "dframe"
  }
  
  # call the server side function
  if(is.null(row.names)){
    cally <-  paste0("dataframeDS(list(",paste(x,collapse=","),"),", 
                     'NULL',",", check.rows,",", check.names,
                     ",list(","'",paste(varnames,collapse="','"),"'","),"
                     ,stringsAsFactors, ")")
  }else{
    cally <-  paste0("dataframeDS(list(",paste(x,collapse=","),"),", 
                     "list(","'",paste(row.names,collapse="','"),"'","),", 
                     check.rows,",", check.names,
                     ",list(","'",paste(varnames,collapse="','"),"'","),"
                     ,stringsAsFactors, ")") 
  }
  datashield.assign(datasources, newobj, as.symbol(cally))
  
  # check that the new object has been created and display a message accordingly
  finalcheck <- isAssigned(datasources, newobj)
  
}