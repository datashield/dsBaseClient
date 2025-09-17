#'
#' @title Performs a mathematical operation on two or more vectors
#' @description Carries out a row-wise operation on two or more vector. The function calls no
#' server side function; it uses the R operation symbols built in DataSHIELD.
#' @details In DataSHIELD it is possible to perform an operation on vectors by just using the relevant
#' R symbols (e.g. '+' for addition, '*' for multiplication, '-' for subtraction and '/' for division).
#' This might however be inconvenient if the number of vectors to include in the operation is large.
#' This function takes the names of two or more vectors and performs the desired operation which could be
#' an addition, a multiplication, a subtraction or a division. If one or more vectors have a missing value
#' at any one entry (i.e. observation), the operation returns a missing value ('NA') for that entry; the output
#' vectors has, hence the same length as the input vectors.
#' @param x a vector of characters, the names of the vectors to include in the operation.
#' @param calc a character, a symbol that indicates the mathematical operation to carry out:
#' '+' for addition, '/' for division, *' for multiplication and '-' for subtraction.
#' @param newobj the name of the output object. By default the name is 'vectorcalc.newobj'.
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} objects obtained after login. If the <datasources>
#' the default set of connections will be used: see \link[DSI]{datashield.connections_default}.
#' @return  no data are returned to user, the output vector is stored on the server side.
#' @author Gaye, A.
#' @export
#' @examples
#' \dontrun{
#'
#'   # load the file that contains the login details
#'   data(logindata)
#'
#'   # login and assign the required variables to R
#'   myvar <- list('LAB_TSC','LAB_HDL')
#'   conns <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#'
#'   # performs an addtion of 'LAB_TSC' and 'LAB_HDL'
#'   myvectors <- c('D$LAB_TSC', 'D$LAB_HDL')
#'   ds.vectorCalc(x=myvectors, calc='+')
#'
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(conns)
#'
#' }
#'
ds.vectorCalc <- function(x=NULL, calc=NULL, newobj=NULL, datasources=NULL){
  .Deprecated("ds.make")
  
  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }

  if(is.null(x)){
    stop("x=NULL. Please provide the names of the objects to combine!", call.=FALSE)
  }

  if(length(x) < 2){
    stop("You must provide the names of at least two vectors!", call.=FALSE)
  }

  # create a name by default if user did not provide a name for the new variable
  if(is.null(newobj)){
    newobj <- "vectorcalc.newobj"
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

  # call the server side function
  cally <- paste0(paste(x,collapse=calc))
  DSI::datashield.assign(datasources, newobj, as.symbol(cally))

}
