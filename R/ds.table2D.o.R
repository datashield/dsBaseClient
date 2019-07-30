#' 
#' @title Generates 2-dimensional contingency tables
#' @description The function ds.table2D.o is a client-side wrapper function. It calls the server-side function 
#' 'table2DDS.o' that generates a 2-dimensional contingency table for each data source. 
#' @details The table returned by the server side function might be valid (non disclosive - no table cell have 
#' counts between 1 and the minimal number agreed by the data owner and set in opal as the "nfilter.tab") or
#' invalid (potentially disclosive - one or more table cells have a count between 1 and the minimal number agreed
#' by the data owner). If a 2-dimensional table is invalid all the cells are set to NA except the total counts.
#' In this way, it is possible to combine total counts across all the data sources but it is not possible to 
#' identify the cell(s) that had the small counts which render the table invalid.
#' @param x a character, the name of a numerical vector with discrete values - usually a factor.
#' @param y a character, the name of a numerical vector with discrete values - usually a factor.
#' @param type a character which represent the type of table to ouput: pooled table or one table for each 
#' data source or both. If \code{type} is set to 'combine', a pooled 2-dimensional table is returned; If \code{type} 
#' is set to 'split' a 2-dimensional table is returned for each data source. If \code{type} is set to 'both' (default)
#' a pooled 2-dimensional table plus a 2-dimensional table for each data source are returned.
#' @param warningMessage a boolean, if set to TRUE (deafult) a warning is displayed if any returned table is invalid. Warning
#' messages are suppressed if this parameter is set to FALSE. However the analyst can still view 'validity' information 
#' which are stored in the output object 'validity' - see the list of output objects.
#' @param datasources a list of opal object(s) obtained after login in to opal servers; these objects hold also the data 
#' assign to R, as \code{dataframe}, from opal datasources. 
#' @return A list object containing the following items:
#' \item{colPercent}{table(s) that hold column percentages for each level/category. Inner cells are reported as
#' missing if one or more cells are 'invalid'.}
#' \item{rowPercent}{table(s) that hold row percentages for each level/category. Inner cells are reported as
#' missing if one or more cells are 'invalid'.}
#' \item{chi2Test}{Chi-squared test for homogeneity.}
#' \item{counts}{table(s) that hold counts for each level/category. If some cell counts are invalid (see 'Details' 
#' section) only the total (outer) cell counts are displayed in the returned individual study tables or in the pooled 
#' table.}
#' \item{validity}{a text that informs the analyst about the validity of the output tables. If any tables are invalid the
#' studies they are originated from are also mentioned in the text message.}
#' @author Amadou Gaye, Paul Burton, Demetris Avraam, for DataSHIELD Development Team
#' @seealso \link{ds.table1D} for the tabulating one vector.
#' @export
#' @examples
#' \dontrun{
#' 
#'   # load the file that contains the login details
#'   data(logindata)
#' 
#'   # login and assign all the variables to R
#'   opals  <-  datashield.login(logins=logindata,assign=TRUE)
#' 
#'   # Example 1: generate a two dimensional table, outputting combined contingency 
#'   # tables - default behaviour
#'   output <- ds.table2D(x='D$DIS_DIAB', y='D$GENDER')
#'   # display the 5 results items, one at a time to avoid having too much information
#'   # displayed at the same time
#'   output$counts
#'   output$rowPercent
#'   output$colPercent
#'   output$chi2Test
#'   output$validity
#' 
#'   # Example 2: generate a two dimensional table, outputting study specific contingency tables
#'   ds.table2D(x='D$DIS_DIAB', y='D$GENDER', type='split')
#'   # display the 5 results items, one at a time to avoid having too much information displayed 
#'   at the same time
#'   output$counts
#'   output$rowPercent
#'   output$colPercent
#'   output$chi2Test
#'   output$validity
#' 
#'   # Example 3: generate a two dimensional table, outputting combined contingency tables 
#'   # *** this example shows what happens when one or studies return an invalid table ***
#'   output <- ds.table2D(x='D$DIS_CVA', y='D$GENDER', type='combine')
#'   output$counts
#'   output$rowPercent
#'   output$colPercent
#'   output$chi2Test
#'   output$validity
#' 
#'   # Example 4: same example as above but output is given for each study,
#'   # separately (i.e. type='split')
#'   # *** this example shows what happens when one or studies return an invalid table ***
#'   output <- ds.table2D(x='D$DIS_CVA', y='D$GENDER', type='split')
#'   output$counts
#'   output$rowPercent
#'   output$colPercent
#'   output$chi2Test
#'   output$validity
#' 
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(opals)
#' 
#' }
#' 
ds.table2D.o <- function(x=NULL, y=NULL, type='both', warningMessage=TRUE, datasources=NULL){ 
  
  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }
  
  if(is.null(x)){
    stop("Please provide the name of the x vector!", call.=FALSE)
  }
  
  if(is.null(y)){
    stop("Please provide the name of the y vector!", call.=FALSE)
  }
  
  if(!(type=="combine" || type=="split" || type=="both")){
    stop("Function argument 'type' has to be either 'combine', 'split' or 'both'")
  }
  
  # the input variable might be given as column table (i.e. D$x)
  # or just as a vector not attached to a table (i.e. x)
  # we have to make sure the function deals with each case
  xnames <- extract(x)
  ynames <- extract(y)
  xvarname <- xnames$elements
  yvarname <- ynames$elements
  xobj2lookfor <- xnames$holders
  yobj2lookfor <- ynames$holders
  xvariable <- xvarname
  yvariable <- yvarname
  
  # check if the input object(s) is(are) defined in all the studies
  if(is.na(xobj2lookfor)){
    defined <- isDefined(datasources, xvarname)
  }else{
    defined <- isDefined(datasources, xobj2lookfor)
  }
  if(is.na(yobj2lookfor)){
    defined <- isDefined(datasources, yvarname)
  }else{
    defined <- isDefined(datasources, yobj2lookfor)
  }
  
  # call the internal function that checks the input object is of the same class in all studies.
  # typ1 <- checkClass(datasources, x)
  # typ2 <- checkClass(datasources, y)
  
  # names of the studies 
  stdnames <- names(datasources)
  
  # call the server side function that produces a 1-dimensional table for each study
  cally <- paste0("table2DDS.o(", x, ",", y, ")")
  output <- opal::datashield.aggregate(datasources, as.symbol(cally))
  
  # extract contingency (count) tables and validity information for each study
  countTables <- vector("list", length(stdnames))
  validityInfo <- vector("list", length(stdnames))
  for(i in 1:length(stdnames)){
    countTables[[i]] <- output[[i]][[1]] 
    validityInfo[[i]] <- output[[i]][[2]]
  }
  names(countTables) <- paste0(stdnames,"-",paste0(x,"(row)|",y,"(col)"))
  names(validityInfo) <- stdnames  
  
  # check if any study is invalid - if yes then only add up the outer columns (i.e. the total)
  invalids <- which(unlist(validityInfo) == "invalid table - invalid counts present")
  
  # pooled counts table
  pooledCounts <- countTables[[1]]
  pooledCounts[] <- 0
  for(i in 1:length(stdnames)){
    pooledCounts <- pooledCounts + countTables[[i]]
  }
  
  # pooled row and column percentage tables
  pooledRowPercent <- rowPercent(pooledCounts)
  pooledColPercent <- colPercent(pooledCounts)
  
  # if a table is invalid in one or more studies alter the pooled tables chi-sqaured test output accordingly
  if(length(invalids > 0)){
    pooledCounts[1:(dim(pooledCounts)[1]-1), 1:(dim(pooledCounts)[2]-1)] <- NA
    pooledRowPercent[1:(dim(pooledCounts)[1]-1), 1:(dim(pooledCounts)[2]-1)] <- NA   
    pooledColPercent[1:(dim(pooledCounts)[1]-1), 1:(dim(pooledCounts)[2]-1)] <- NA   
    pooledChi2test <- "Error: all entries of the table must be non-negative and finite"
  }else{
    pooledContingencyTable <- pooledCounts[1:(dim(pooledCounts)[1]-1), 1:(dim(pooledCounts)[2]-1)]
    pooledChi2test <- stats::chisq.test(pooledContingencyTable)
  }
  
  # study specific row and column percentage tables (one for each study)
  rowPercentTables <- vector("list", length(stdnames))
  colPercentTables <- vector("list", length(stdnames))
  for(i in 1:length(stdnames)){
    rowPercentTables[[i]] <- rowPercent(countTables[[i]])
    colPercentTables[[i]] <- colPercent(countTables[[i]])
  }  
  names(rowPercentTables) <- paste0(stdnames,"-",paste0(x,"(row)|",y,"(col)")) 
  names(colPercentTables) <- paste0(stdnames,"-",paste0(x,"(row)|",y,"(col)")) 
  
  # chi-squared tests for each study separetely
  chi2Tests <- vector("list", length(stdnames))
  for(i in 1:length(stdnames)){
    if(length(invalids) > 0 & i%in%invalids){
      chi2Tests[[i]] <- "Error: all entries of the table must be non-negative and finite"
    }else{
      contingencyTable <- countTables[[i]][1:(dim(countTables[[i]])[1]-1), 1:(dim(countTables[[i]])[2]-1)]
      options(warn = -1) # suppress warning temporarily to avoid 'nuisance' message, analyst will found out chi2 results anyway.
      chi2Tests[[i]] <- stats::chisq.test(contingencyTable )
      options(warn = 0)  # put warning back
    }
  }
  names(chi2Tests) <- paste0(stdnames,"-",paste0(x,"(row)|",y,"(col)"))   
  
  # return the right output depending what the user specified: 'combine' or 'split' analysis
  if(type=="combine" || type=="both"){
    if(length(invalids > 0)){
      if(warningMessage){
        message(paste0("WARNING: Invalid contingency table from '", paste(stdnames[invalids], collapse=", "), "' !\n         Only total values are returned in the output table(s)."))
      }
      validityValue <- paste0("Invalid contingency table from '", paste(stdnames[invalids], collapse=", "), "'!")
    }else{
      validityValue <- "All tables are valid!"
    }
    # adding labels to pooled outputs before returning them
    pooledCounts <- structure(list(pooledCounts), names=paste0("pooled-",x,"(row)|",y,"(col)"))
    pooledRowPercent <- structure(list(pooledRowPercent), names=paste0("pooled-",x,"(row)|",y,"(col)"))
    pooledColPercent <- structure(list(pooledColPercent), names=paste0("pooled-",x,"(row)|",y,"(col)"))
    pooledChi2test <- structure(list(pooledChi2test), names=paste0("pooled-",x,"(row)|",y,"(col)"))
    out.all.studies <- list(colPercent.all.studies=pooledColPercent, rowPercent.all.studies=pooledRowPercent, chi2Test.all.studies=pooledChi2test, counts.all.studies=pooledCounts, validity=validityValue)
    output.object <- out.all.studies 
  }
  
  if(type=="split" || type=="both"){
    if(length(invalids > 0)){
      if(warningMessage){
        message(paste0("WARNING: Invalid contingency table from '", paste(stdnames[invalids], collapse=", "), "' !\n          Only total values are returned in the output table(s)."))          
      }
      validityValue <- paste0("Invalid contingency table from '", paste(stdnames[invalids], collapse=", "), "'!")
    }else{
      validityValue <- "All tables are valid!"
    }
    out.split <- (list(colPercent=colPercentTables, rowPercent=rowPercentTables, chi2Test=chi2Tests, counts=countTables,  validity=validityValue))
    output.object <- out.split
    if(type=="both"){
      output.object <- list(colPercent=colPercentTables, colPercent.all.studies=pooledColPercent,rowPercent=rowPercentTables,rowPercent.all.studies=pooledRowPercent, 
		 chi2Test=chi2Tests, chi2Test.all.studies=pooledChi2test, counts=countTables, counts.all.studies=pooledCounts, validity=validityValue)
    }
  }		
	
  return(output.object)
  
}
