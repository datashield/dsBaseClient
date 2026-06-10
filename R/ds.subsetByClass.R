#'
#' @title Generates valid subset(s) of a data frame or a factor
#' @description The function takes a categorical variable or a data frame as input and generates
#' subset(s)  variables or data frames for each category.
#' @details If the input data object is a data frame it is possible to specify  the variables
#' to subset on. If a subset is not 'valid' all its the values are reported as missing (i.e. NA),
#' the name of the subsets is labelled with the suffix '_INVALID'. Subsets are considered invalid if
#' the number of observations it holds are between 1 and the threshold allowed by the data owner. if
#' a subset is empty (i.e. no entries) the name of the subset is labelled with the suffix '_EMPTY'.
#' @param x a character, the name of the dataframe or the vector to generate subsets from.
#' @param variables a vector of string characters, the name(s) of the variables to subset by.
#' @param subsets the name of the output object, a list that holds the subset objects. If set to NULL
#' the default name of this list is 'subClasses'.
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} objects obtained after login. If the <datasources>
#' the default set of connections will be used: see \link[DSI]{datashield.connections_default}.
#' @return a no data are return to the user but messages are printed out.
#' @author Gaye, A.
#' @seealso \link{ds.meanByClass} to compute mean and standard deviation across categories of a factor vectors.
#' @seealso \link{ds.subset} to subset by complete cases (i.e. removing missing values), threshold, columns and rows.
#' @export
#' @examples
#' \dontrun{
#'
#'   # load the login data
#'   data(logindata)
#'
#'   # login and assign some variables to R
#'   myvar <- list('DIS_DIAB','PM_BMI_CONTINUOUS','LAB_HDL', 'GENDER')
#'   conns <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#'
#'   # Example 1: generate all possible subsets from the table assigned above (one subset table
#'   # for each class in each factor)
#'   ds.subsetByClass(x='D', subsets='subclasses')
#'   # display the names of the subset tables that were generated in each study
#'   ds.names('subclasses')
#'
#'   # Example 2: subset the table initially assigned by the variable 'GENDER'
#'   ds.subsetByClass(x='D', subsets='subtables', variables='GENDER')
#'   # display the names of the subset tables that were generated in each study
#'   ds.names('subtables')
#'
#'   # Example 3: generate a new variable 'gender' and split it into two vectors: males
#'   # and females
#'   ds.assign(toAssign='D$GENDER', newobj='gender')
#'   ds.subsetByClass(x='gender', subsets='subvectors')
#'   # display the names of the subset vectors that were generated in each study
#'   ds.names('subvectors')
#'
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(conns)
#'
#' }
#'
ds.subsetByClass <- function(x=NULL, subsets="subClasses", variables=NULL, datasources=NULL){
  .Deprecated("ds.dataFrameSubset")

  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }

  if(is.null(x)){
    stop("Please provide the name of the input data frame or factor!", call.=FALSE)
  }

  # check if the input x is defined in all the studies
  defined <- isDefined(datasources, x)

  # call the internal function that checks the input object is of the same class in all studies.
  typ <- checkClass(datasources, x)

  # the input object must be a dataframe or a factor
  if(!('data.frame' %in% typ) & !('factor' %in% typ)){
    message(paste0(x, " is of type ", typ, "!"))
    stop("The object to subset from must be a 'data.frame' or a 'factor'.", call.=FALSE)
  }

  # number of studies and their names
  numsources <- length(datasources)
  stdnames <- names(datasources)

  # check that, for each study,  all the columns of the input table are of 'numeric' type
  if(typ == 'data.frame'){
    dtname <- x
    for(i in 1:numsources){
      cols <- DSI::datashield.aggregate(datasources[i], call("colnamesDS", x))
      dims <- DSI::datashield.aggregate(datasources[i], call("dimDS", x))
      tracker <-c()
      for(j in 1:dims[[1]][2]){
        cally <- call("classDS", paste0(dtname, "$", cols[[1]][j]))
        res <- DSI::datashield.aggregate(datasources[i], cally)
        if(res[[1]] != 'factor'){
          tracker <- append(tracker, 0)
        }else{
          tracker <- append(tracker, 1)
        }
      }
      if(sum(tracker) == 0){
        stop("No factor variable found in data frame ", dtname, " in ",  stdnames[i], ".", call.=FALSE)
      }
    }
  }

  # call the server side function that does the job
  # get the indices of the columns referred to by their names in the arguments
  if(is.null(variables)){
    cally <- paste0("subsetByClassDS('", x, "')")
  }else{
    cally <- paste0("subsetByClassDS('", x, "', c('",paste(variables,collapse="','"),"'))")
  }
  DSI::datashield.assign(datasources, subsets, as.symbol(cally))

  # check that the new object has been created and display a message accordingly
  finalcheck <- isAssigned(datasources, subsets)

}
