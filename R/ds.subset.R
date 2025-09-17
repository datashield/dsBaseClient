#'
#' @title Generates a valid subset of a table or a vector
#' @description The function uses the R classical subsetting with squared brackets '[]' and allows also to
#' subset using a logical operator and a threshold. The object to subset from must be a vector (factor, numeric
#' or character) or a table (data.frame or matrix).
#' @details (1) If the input data is a table the user specifies the rows and/or columns to include in the subset; the columns can be
#' referred to by their names. Table subsetting can also be done using the name of a variable and a threshold (see example 3).
#' (2) If the input data is a vector and the parameters 'rows', 'logical' and 'threshold' are all provided the last two are ignored
#' (i.e. 'rows' has precedence over the other two parameters then).
#' IMPORTANT NOTE: If the requested subset is not valid (i.e. contains less than the allowed number of observations) all the values are
#' turned into missing values (NA). Hence an invalid subset is indicated by the fact that all values within it are set to NA.
#' @param subset the name of the output object, a list that holds the subset object. If set to NULL
#' the default name of this list is 'subsetObject'
#' @param x a character, the name of the dataframe or the factor vector and the range of the subset.
#' @param completeCases a character that tells if only complete cases should be included or not.
#' @param rows a vector of integers, the indices of the rows to extract.
#' @param cols a vector of integers or a vector of characters; the indices of the columns to extract or their names.
#' @param logicalOperator a boolean, the logical parameter to use if the user wishes to subset a vector using a logical
#' operator. This parameter is ignored if the input data is not a vector.
#' @param threshold a numeric, the threshold to use in conjunction with the logical parameter. This parameter is ignored
#' if the input data is not a vector.
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} objects obtained after login. If the <datasources>
#' the default set of connections will be used: see \link[DSI]{datashield.connections_default}.
#' @return no data are return to the user, the generated subset dataframe is stored on the server side.
#' @author Gaye, A.
#' @seealso \link{ds.subsetByClass} to subset by the classes of factor vector(s).
#' @seealso \link{ds.meanByClass} to compute mean and standard deviation across categories of a factor vectors.
#' @export
#' @examples
#' \dontrun{
#'
#'   # load the login data
#'   data(logindata)
#'
#'   # login and assign some variables to R
#'   myvar <- list("DIS_DIAB","PM_BMI_CONTINUOUS","LAB_HDL", "GENDER")
#'   conns <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#'
#'   # Example 1: generate a subset of the assigned dataframe (by default the table is named 'D')
#'   # with complete cases only
#'   ds.subset(x='D', subset='subD1', completeCases=TRUE)
#'   # display the dimensions of the initial table ('D') and those of the subset table ('subD1')
#'   ds.dim('D')
#'   ds.dim('subD1')
#'
#'   # Example 2: generate a subset of the assigned table (by default the table is named 'D')
#'   # with only the variables
#'   # DIS_DIAB' and'PM_BMI_CONTINUOUS' specified by their name.
#'   ds.subset(x='D', subset='subD2', cols=c('DIS_DIAB','PM_BMI_CONTINUOUS'))
#'
#'   # Example 3: generate a subset of the table D with bmi values greater than or equal to 25.
#'   ds.subset(x='D', subset='subD3', logicalOperator='PM_BMI_CONTINUOUS>=', threshold=25)
#'
#'   # Example 4: get the variable 'PM_BMI_CONTINUOUS' from the dataframe 'D' and generate a
#'   # subset bmi
#'   # vector with bmi values greater than or equal to 25
#'   ds.assign(toAssign='D$PM_BMI_CONTINUOUS', newobj='BMI')
#'   ds.subset(x='BMI', subset='BMI25plus', logicalOperator='>=', threshold=25)
#'
#'   # Example 5: subsetting by rows:
#'   # get the logarithmic values of the variable 'lab_hdl' and generate a subset with
#'   # the first 50 observations of that new vector. If the specified number of row is
#'   # greater than the total
#'   # number of rows in any of the studies the process will stop.
#'   ds.assign(toAssign='log(D$LAB_HDL)', newobj='logHDL')
#'   ds.subset(x='logHDL', subset='subLAB_HDL', rows=c(1:50))
#'   # now get a subset of the table 'D' with just the 100 first observations
#'   ds.subset(x='D', subset='subD5', rows=c(1:100))
#'
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(conns)
#'
#' }
#'
ds.subset <- function(x=NULL, subset="subsetObject", completeCases=FALSE, rows=NULL, cols=NULL, logicalOperator=NULL, threshold=NULL, datasources=NULL){
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
    stop("Please provide the name of the object to subset from!", call.=FALSE)
  }

  # check if the input x is defined in all the studies
  defined <- isDefined(datasources, x)

  # call the internal function that checks the input object is of the same class in all studies.
  typ <- checkClass(datasources, x)

  # the input object must be a dataframe or a vector
  if(!('data.frame' %in% typ) & !('character' %in% typ) & !('factor' %in% typ) & !('integer' %in% typ) & !('logical' %in% typ) & !('numeric' %in% typ)){
    stop("The object to subset from must be a 'data.frame', a 'character', factor', a 'logical' or a 'numeric' vector.", call.=FALSE)
  }

  # the argument 'rows' and 'cols' have precedence over 'logicalOperator' and 'threshold'
  if(is.null(rows) & is.null(cols)){
    if(is.null(logicalOperator) | is.null(threshold)){
      if(!completeCases){
        stop("No subset parameters provided, the sought subset is the same as the original object!", call.=FALSE)
      }else{
        cally <- call('subsetDS', x, completeCases)
        DSI::datashield.assign(datasources, subset, cally)
      }
    }else{
      # allow this only for numeric vectors
      if(("factor" %in% typ) | ("character" %in% typ)){
        stop(" Subsetting on a threshold criteria is allowed only for numeric vectors!", call.=FALSE)
      }
      # get the logicalOperator operator and any variable provided with it
      lg <- unlist(strsplit(logicalOperator, split=""))
      var2sub <- NULL
      if(lg[length(lg)] == "=" & lg[(length(lg)-1)] == ">" | lg[(length(lg)-1)] == "<" | lg[(length(lg)-1)] == "=" |  lg[(length(lg)-1)] == "!"){
        logicalOperator <- paste0(lg[(length(lg)-1)], lg[length(lg)])
        if(length(lg) > 2){ var2sub <- paste(lg[1:(length(lg)-2)], collapse="") }
      }else{
        logicalOperator <- lg[length(lg)]
        if(length(lg) > 1) { var2sub <- paste0(lg[1:(length(lg)-1)], collapse="") }
      }

      # turn the logicalOperator operator into the corresponding integer that will be evaluated on the server side.
      logicalOperator <- logical2int(logicalOperator)
      cally <- call('subsetDS', x, completeCases, rows, cols, logicalOperator, threshold, var2sub)
      DSI::datashield.assign(datasources, subset, cally)
    }
  }else{

    # check if the sought subset is not larger than the vector or the table size
    # if not call the server side function and carry out the subsetting
    if(("factor" %in% typ) | ("numeric" %in% typ) | ("character" %in% typ)){
      # if the size of the requested subset is greater than that of original set the rows or cols to NULL
      # these will then be set to the maximum size in the server side
      if(!(is.null(rows))){
        ll <- DSI::datashield.aggregate(datasources, call("lengthDS", x))
        for(i in 1:length(datasources)){
          if(length(rows) > ll[[i]]){
            rows <- NULL
          }
        }
      }
      # turn the vector of row indices into a character to pass the parser
      for(i in 1:length(datasources)){
        cally <- call('subsetDS', x, completeCases, rows)
        DSI::datashield.assign(datasources[i], subset, cally)
      }
    }else{
      if(("data.frame" %in% typ) | ("matrix" %in% typ)){
        # call the internal function that ensure wrong size subset is not requested (i.e. subset size > original table)
        for(i in 1:length(datasources)){
          check00 <- subsetHelper(datasources[i], x, rows, cols)
          if(check00[1] == 1){ rows <- NULL }
          if(check00[2] == 1){ cols <- NULL }
        }
        for(i in 1:length(datasources)){
          cally <- call('subsetDS', x, completeCases, rows, cols)
          DSI::datashield.assign(datasources[i], subset, cally)
        }
      }else{
        stop("The object to subset from must be a numeric, character or factor vector or a table structure (matrix or data.frame).", call.=FALSE)
      }
    }
  }

}
