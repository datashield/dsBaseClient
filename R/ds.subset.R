#' 
#' @title Generates a valid subset of a table or a vector
#' @description The function uses the R classical subsetting with squared brackets '[]' and allows also to 
#' subset using a logical oprator and a threshold. The object to subset from must be a vector (factor, numeric 
#' or charcater) or a table (data.frame or matrix).
#' @details The user specifies the rows and/or columns to include in the subset if the input 
#' object is a table; the columns can be refered to by their names. Only the parameter 'rows' is required if the input
#' object is a vector; The name of a vector (i.e. a variable) can also be provided with a logical
#' operator and a threshold. If the input data is a vector and the parameters 'rows', 'logical' and 'threshold' are all
#' provided the last two are ignored. If the requested subset is not valid (i.e. contains less than the allowed
#' number of observations), the subset is not generated, rather a table or a vector of missing values is generated to allow
#' for any subsequent process using the output of the function to proceed after informing the user via a message.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @param subset the name of the output object, a list that holds the subset object. If set to NULL
#' the default name of this list is 'subsetObject' 
#' @param data a string character, the name of the dataframe or the factor vector and the range of the subset.
#' @param rows a vector of integers, the indices of the rows de extract. 
#' @param cols a vector of integers or characters; the indices of the columns to extract or the names of the columns (i.e. 
#' names of the variables to extract).
#' @param logical a charcater, the logical parameter to use if the user wishes to subset a vector using a logical 
#' operator. This parameter is ignored if the input data is not a vector.
#' @param threshold a numeric, the threshold to use in conjunction with the logical parameter. This parameter is ignored 
#' if the input data is not a vector.
#' @return a no data are return to the user but messages are printed out.
#' @author Gaye, A.
#' @export
#' @examples {
#'
#' # load the login data
#' library(opal)
#' data(logindata)
#' 
#' # login and assign some variables to R
#' myvar <- list("DIS_DIAB","PM_BMI_CONTINUOUS","LAB_HDL", "GENDER")
#' opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#' # Example 1: generate a subset of the assigned table (by default the table is named 'D')
#' # with the first 50 observations and the two first columns
#' ds.subset(datasources=opals, subset="subD", data="D", rows=c(1:50), cols <- c(1,2))
#' 
#' # Example 2: generate a subset of the assigned table (by default the table is named 'D')
#' # with the first 50 observations and the two first columns refered to by their names.
#' ds.subset(datasources=opals, subset="subD", data="D", rows=c(1:50), cols <- c("DIS_DIAB","PM_BMI_CONTINUOUS"))
#'
#' # Example 3: generate a subset of the table D with bmi values greater than or equal to 25.
#' ds.subset(datasources=opals, subset="subBMI", data="D", logical="PM_BMI_CONTINUOUS>=", threshold=25)
#' 
#' # Example 4: get the logarithmic values of the variable 'lab_hdl' and generate a subset with 
#' the first 50 observations of that new vector.
#' ds.assign(opals, "logHDL", "log(D$LAB_HDL)")
#' ds.subset(datasources=opals, subset="subLAB_HDL", data="logHDL", rows=c(1:50))
#' 
#' # Example 5: get the variable 'PM_BMI_CONTINUOUS' from the dataframe 'D' and generate a subset bmi
#' vector with bmi values greater than or equal to 25
#' ds.assign(opals, "BMI", "D$PM_BMI_CONTINUOUS")
#' ds.subset(datasources=opals, subset="subBMI", data="BMI", logical=">=", threshold=25)
#' 
#' }
#' 
ds.subset <- function(datasources=NULL, subset="subset", data=NULL, rows=NULL, cols=NUL, logical=NULL, threshold=NULL){
  
  if(is.null(datasources)){
    message("No valid opal object(s) provided!")
    message("Make sure you are logged in to valid opal server(s).")
    stop(" End of process!", call.=FALSE)
  }else{
    stdnames <- names(datasources)
  }
  
  # check in the input data is defined in all sudies
  ch1 <- unique(unlist(ds.exists(datasources, data)))
  if(mean(ch1, na.rm=TRUE) != 1){
    message("The input data is not defined in one or more studies; use the function 'ds.exists' to check further.")
    stop(" End of process!", call.=FALSE)
  }
  
  if(is.null(rows) & is.null(cols) & is.null(logical) & is.null(threshold)){
    message("None of the subsetting parameters is provided!")
    stop(" End of process!", call.=FALSE)
  }
  
  # call the internal function that checks the input data is of the same class in all studies.
  typ <- dsbaseclient:::.checkClass(datasources, data)
  
  # stop the process if the input data is not a table or a vector
  if(typ != "factor" & typ != "numeric" & typ != "character"  & typ != "matrix" & typ != "data.frame"){
    stop("The object to subset from must be a numeric, character or factor vector or a table (matrix or data.frame).", call.=FALSE)
  }
  
  # check if the provided range is not larger than the vector or the table size
  # if not call the server side function and carry out the subsetting
  if(typ == "factor" | typ == "numeric" | typ == "character"){
    if(!(is.null(rows))){
      # if the size of the requested subset is greater than that of original inform the user and stop the process
      ll <- ds.length(datasources, data)
      fail <- 0
      for(i in 1:length(datasources)){
        if(length(rows) > ll[[i]]){
          fail <- append(fail, i)        
        }
      }
      if(sum(fail) > 0){
        stop(paste0("The subset you specified is larger than ", data, " in ", paste(stdnames[fail], collapse=", "), "."))
      }else{
        # turn the vector of row indices into a character to pass the parser
        invect <- as.character(rows)
        cally <- call('subsetDS', data, invect)
        datashield.assign(datasources, cally)
      }
    }else{
      if(!(is.null(logical)) & !(is.null(threshold))){
        # turn the logical operator into the corresponding integer that will be evaluated on the server side.
        logical <- dsbaseclient:::.logical2int(logical)
        cally <- call('subsetDS', data, logical, threshold)
        datashield.assign(datasources, cally)
      }else{
        stop("Please provide criteria to subset the vector: set 'rows' or 'logical' and 'threshold'", call.=FALSE)
      }
    }
  }else{
    if(!(is.null(rows) | !(is.null(cols)))){
      # call the internal function that ensure wrond size subset is not requested (i.e. subset size > original table)
      dsbaseclient:::.subsetHelper(datasources, data, rows, cols)
    }else{
      if(!(is.null(logical)) & !(is.null(threshold))){
        # turn the logical operator into the corresponding integer that will be evaluated on the server side.
        logical <- dsbaseclient:::.logical2int(logical)
        cally <- call('subsetDS', data, logical, threshold)
        datashield.assign(datasources, cally)
      }else{
        stop("Please provide criteria to subset the table: set 'rows' and/or 'cols' or 'logical' and 'threshold'", call.=FALSE)
      }
    }
  }
  
  # a message so the user knows the function was ran (assign functions are 'silent')
  message("An 'assign' function was ran, no output should be expected on the client side!\n")
  
  # possible 'errors' from the server side function if no subset has been generated
  m1 <- "Invalid subset vector"
  m2 <- "Invalid subset table"
  txt2print <- c(m1, m2)
  
  # check the subsets and tell if they have been generated or if some are invalid 
  for(i in 1: length(datasources)){
    listcontent <- ds.names(datasources[i], subset)
    if(listcontent[[1]][1] == m1 | listcontent[[1]][1] == m2 | listcontent[[1]][1] == m3){
      for(q in 1:3){
        if(listcontent[[1]][1] == txt2print[q]){
          message(paste0(txt2print[q], " in ", stdnames[i]))
        }
      }
    }
  }
}
