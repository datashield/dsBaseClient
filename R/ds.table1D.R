#'
#' @title Generates 1-dimensional contingency tables
#' @description The function ds.table1D is a client-side wrapper function.
#' It calls the server-side function table1DDS to generate 1-dimensional
#' tables for all data sources.
#' @details The table returned by the server side function might be valid (non disclosive -
#' no table cell have counts between 1 and the minimal number agreed by the data owner and set in the data repository)
#' or invalid (potentially disclosive - one or more table cells have a count between 1 and the minimal number
#' agreed by the data owner). If a 1-dimensional table is invalid all the cells are set to NA except the total
#' count. This way it is possible the know the total count and combine total counts across data sources but it
#' is not possible to identify the cell(s) that had the small counts which render the table invalid.
#' @param x a character, the name of a numerical vector with discrete values - usually a factor.
#' @param type a character which represent the type of table to output: pooled table or one table for each
#' data source. If \code{type} is set to 'combine', a pooled 1-dimensional table is returned; if If \code{type}
#' is set to 'split' a 1-dimensional table is returned for each data source.
#' @param warningMessage a boolean, if set to TRUE (default) a warning is displayed if any returned table is invalid. Warning
#' messages are suppressed if this parameter is set to FALSE. However the analyst can still view 'validity' information
#' which are stored in the output object 'validity' - see the list of output objects.
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} objects obtained after login. If the <datasources>
#' the default set of connections will be used: see \link[DSI]{datashield.connections_default}.
#' @return A list object containing the following items:
#' \item{counts}{ table(s) that hold counts for each level/category. If some cells counts are invalid (see 'Details'
#' section) only the total (outer) cell counts are displayed in the returned individual study tables or in the pooled
#' table.}
#' \item{percentages}{ table(s) that hold percentages for each level/category. Here also inner cells are reported as
#' missing if one or more cells are 'invalid'.}
#' \item{validity}{ a text that informs the analyst about the validity of the output tables. If any tables are invalid the
#' studies they are originated from are also mentioned in the text message.}
#' @author Gaye, A.; Burton, P.
#' @seealso \link{ds.table2D} for cross-tabulating two vectors.
#' @export
#' @examples
#' \dontrun{
#'
#'   # load the file that contains the login details
#'   data(logindata)
#'
#'   # login and assign all the stored variables to R
#'   conns <- datashield.login(logins=logindata,assign=TRUE)
#'
#'   # Example 1: generate a one dimensional table, outputting combined (pooled) contingency tables
#'   output <- ds.table1D(x='D$GENDER')
#'   output$counts
#'   output$percentages
#'   output$validity
#'
#'   # Example 2: generate a one dimensional table, outputting study specific contingency tables
#'   output <- ds.table1D(x='D$GENDER', type='split')
#'   output$counts
#'   output$percentages
#'   output$validity
#'
#'   # Example 3: generate a one dimensional table, outputting study specific and combined
#'   # contingency tables - see what happens if the reruened table is 'invalid'.
#'   output <- ds.table1D(x='D$DIS_CVA')
#'   output$counts
#'   output$percentages
#'   output$validity
#'
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(conns)
#'
#' }
#'
ds.table1D <- function(x=NULL, type='combine', warningMessage=TRUE, datasources=NULL){
  .Deprecated("ds.table")

  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }

  if(is.null(x)){
    stop("Please provide the name of the input vector!", call.=FALSE)
  }

  # the input variable might be given as column table (i.e. D$x)
  # or just as a vector not attached to a table (i.e. x)
  # we have to make sure the function deals with each case
  xnames <- extract(x)
  varname <- xnames$elements
  obj2lookfor <- xnames$holders
  variable <- varname

  # check if the input object(s) is(are) defined in all the studies
  if(is.na(obj2lookfor)){
    defined <- isDefined(datasources, varname)
  }else{
    defined <- isDefined(datasources, obj2lookfor)
  }

  # call the internal function that checks the input object is of the same class in all studies.
  typ <- checkClass(datasources, x)

  # names of the studies
  stdnames <- names(datasources)

  # call the server side function that produces a 1-dimensional table for each study
  cally <- paste0("table1DDS(", x, ")")
  output <- DSI::datashield.aggregate(datasources, as.symbol(cally))

  # extract contingency (count) tables and validity information for each study
  countTables <- vector("list", length(stdnames))
  validityInfo <- vector("list", length(stdnames))
  for(i in 1:length(stdnames)){
    countTables[[i]] <- t(output[[i]][[1]])
    colnames(countTables[[i]]) <- x
    validityInfo[[i]] <- output[[i]][[2]]
  }
  names(countTables) <- stdnames
  names(validityInfo) <- stdnames

  # first check if any study is invalid - if yes then only add up the outer columns (i.e. the total)
  invalids <- which(unlist(validityInfo) == "invalid table - invalid counts present")

  # generate the pooled counts table
  pooledCounts <- countTables[[1]]
  pooledCounts[,1] <- 0
  for(i in 1:length(stdnames)){
    pooledCounts <- pooledCounts + countTables[[i]]
  }
  if(length(invalids > 0)){
    pooledCounts[1:(dim(pooledCounts)[1])-1, 1] <- NA
  }

  # percentage tables (one for each study)
  percentageTables <- vector("list", length(stdnames))
  for(i in 1:length(stdnames)){
    temp <- countTables[[i]]
    totalIdx <- dim(countTables[[i]])[1]
    temp[,1] <- round((countTables[[i]][,1]/countTables[[i]][totalIdx,1])*100,2)
    percentageTables[[i]]  <- temp
  }
  names(percentageTables) <- stdnames

  # generate the pooled counts table
  if(length(invalids > 0)){
    pooledPercentages <- percentageTables[[1]]
    pooledPercentages[1:(dim(pooledCounts)[1])-1, 1] <- NA
  }else{
    pooledPercentages <- round((pooledCounts/pooledCounts[dim(pooledCounts)[1], 1])*100,2)
  }

  # return the right output depending what the user specified: 'combine' or 'split' analysis
  if(type=="combine"){
    if(length(invalids > 0)){
      if(warningMessage){
        message(paste0("WARNING: Invalid tables from '", paste(stdnames[invalids], collapse=", "), "' !\n         Only total values are returned in the output table(s)."))
      }
      validityValue <- paste0("Invalid tables from '", paste(stdnames[invalids], collapse=", "), "'!")
    }else{
      validityValue <- "All tables are valid!"
    }
    return(list(counts=pooledCounts, percentages=pooledPercentages, validity=validityValue))
  }else{
    if(type=="split"){
      if(length(invalids > 0)){
        if(warningMessage){
          message(paste0("WARNING: Invalid table(s) from '", paste(stdnames[invalids], collapse=", "), "' !\n          Only total values are returned in the output table(s)."))
        }
        validityValue <- paste0("Invalid table(s) from '", paste(stdnames[invalids], collapse=", "), "'!")
      }else{
        validityValue <- "All tables are valid!"
      }
        return(list(counts=countTables, percentages=percentageTables, validity=validityValue))
    }else{
      stop("Function argument 'type' has to be either 'combine' or 'split'")
    }
  }
}
