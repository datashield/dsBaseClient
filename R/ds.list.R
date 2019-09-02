#'
#' @title Function to construct a list object
#' @description This is similar to the R function \code{list}.
#' @details If the objects to coerce into a list are for example vectors held in a matrix
#' or a dataframe the names of the elements in the list are the names of columns.
#' @param x a character, the names of the objects to coerce into a list.
#' @param newobj the name of the output object. If this argument is set to \code{NULL},
#' the name of the new object is 'newlist'.
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login. If the <datasources>
#' the default set of connections will be used: see \link{datashield.connections_default}.
#' @return  nothing is returned to the client, the new object is stored on the server side.
#' @author Gaye, A.; Isaeva, J.
#' @export
#' @examples
#' \dontrun{
#'
#'  # load the file that contains the login details
#'  data(logindata)
#'
#'  # login and assign the required variables to R
#'  myvar <- list("LAB_TSC","LAB_HDL")
#'  conns <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#'
#'  # combine the 'LAB_TSC' and 'LAB_HDL' variables into a list
#'  myobjects <- c('D$LAB_TSC', 'D$LAB_HDL')
#'  ds.list(x=myobjects)
#'
#'  # clear the Datashield R sessions and logout
#'  datashield.logout(conns)
#'
#' }
#'
ds.list = function(x=NULL, newobj=NULL, datasources=NULL){

  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  if(is.null(x)){
    stop("x=NULL. Please provide the names of the objects to coerce into a list!", call.=FALSE)
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

  # create a name by default if user did not provide a name for the new variable
  if(is.null(newobj)){
    newobj <- "newlist"
  }

  # get the names of the list elements if the user has not specified any
  if(is.null(names)){
    names <- varnames
  }

  # call the server side function that does the job
  cally <-  paste0("listDS(list(",paste(x,collapse=","),"), list('",paste(varnames,collapse="','"),"'))")
  DSI::datashield.assign(datasources, newobj, as.symbol(cally))

  # check that the new object has been created and display a message accordingly
  finalcheck <- isAssigned(datasources, newobj)

}
