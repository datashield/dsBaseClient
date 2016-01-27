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
#' @param completeCases a boolean that tells if only complete cases should be included or not.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @return  nothing is returned to the client, the new object is stored on the server side.
#' @author Gaye, A.; Isaeva, J.
#' @seealso \link{ds.cbind} Combines objects column-wise.
#' @seealso \link{ds.changeRefGroup} to change the reference level of a factor.
#' @seealso \link{ds.colnames} to obtain the column names of a matrix or a data frame
#' @seealso \link{ds.asMatrix} to coerce an object into a matrix type.
#' @seealso \link{ds.dim} to obtain the dimensions of matrix or a data frame.
#' @export
#' @examples {
#'   
#'   # load the file that contains the login details
#'   data(logindata)
#'   library(opal)
#'
#'   # login and assign the required variables to R
#'   myvar <- list('LAB_TSC','LAB_HDL')
#'   opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#'   # create a dataframe that contains the variables the 'LAB_TSC' and 'LAB_HDL'
#'   # all the arguments are set to default in this example
#'   myvectors <- c('D$LAB_TSC', 'D$LAB_HDL')
#'   ds.dataframe(x=myvectors)
#' 
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(opals)
#' 
#' }
#' 
ds.dataframe = function(x=NULL,newobj=NULL,row.names=NULL,check.rows=FALSE,check.names=TRUE,stringsAsFactors=TRUE,completeCases=FALSE,datasources=NULL){
  
  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
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
                     ,stringsAsFactors,",",completeCases,")")
  }else{
    cally <-  paste0("dataframeDS(list(",paste(x,collapse=","),"),", 
                     "list(","'",paste(row.names,collapse="','"),"'","),", 
                     check.rows,",", check.names,
                     ",list(","'",paste(varnames,collapse="','"),"'","),"
                     ,stringsAsFactors,",",completeCases,")") 
  }
  opal::datashield.assign(datasources, newobj, as.symbol(cally))
  
  # check that the new object has been created and display a message accordingly
  finalcheck <- isAssigned(datasources, newobj)
  
}