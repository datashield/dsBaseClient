#' 
#' @title Generates a valid subset of a table or a vector
#' @description The function uses the R classical subsetting with squared brackets '[]' and allows also to 
#' subset using a logical oprator and a threshold. The object to subset from must be a vector (factor, numeric 
#' or charcater) or a table (data.frame or matrix).
#' @details If the input data is a table: The user specifies the rows and/or columns to include in the subset if the input 
#' object is a table; the columns can be refered to by their names. The name of a vector (i.e. a variable) can also be provided 
#' with a logical operator and a threshold (see example 3). 
#' If the input data is a vector:  when the parameters 'rows', 'logical' and 'threshold' are all provided the last two are ignored 
#' ('rows' has precedence over the other two parameters then).
#' IMPORTANT NOTE: If the requested subset is not valid (i.e. contains less than the allowed number of observations) or if it does 
#' not have any observations, the subset is not generated, rather a table or a vector of missing values is generated to allow
#' for any subsequent process using the output of the function to proceed, after informing the user via a message.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @param subset the name of the output object, a list that holds the subset object. If set to NULL
#' the default name of this list is 'subsetObject' 
#' @param data a string character, the name of the dataframe or the factor vector and the range of the subset.
#' @param complete a boolean that tells if only complete cases should be included or not.
#' @param rows a vector of integers, the indices of the rows de extract. 
#' @param cols a vector of integers or characters; the indices of the columns to extract or the names of the columns (i.e. 
#' names of the variables to extract).
#' @param logical a charcater, the logical parameter to use if the user wishes to subset a vector using a logical 
#' operator. This parameter is ignored if the input data is not a vector.
#' @param threshold a numeric, the threshold to use in conjunction with the logical parameter. This parameter is ignored 
#' if the input data is not a vector.
#' @return a no data are return to the user, the generated subset is stored on the server side.
#' @author Gaye, A.
#' @export
#' @examples {
#'
#' # load the login data
#' data(logindata)
#' 
#' # login and assign some variables to R
#' myvar <- list("DIS_DIAB","PM_BMI_CONTINUOUS","LAB_HDL", "GENDER")
#' opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#' # Example 1: generate a subset of the assigned dataframe (by default the table is named 'D') with complete cases only
#' ds.subset(datasources=opals, subset='subD', data='D', complete=TRUE)
#' 
#' # Example 2: generate a subset of the assigned table (by default the table is named 'D') with only two variables 
#' # specified by their names
#' ds.subset(datasources=opals, subset='subD', data='D', cols <- c('DIS_DIAB','PM_BMI_CONTINUOUS'))
#'
#' # Example 3: generate a subset of the table D with bmi values greater than or equal to 25.
#' ds.subset(datasources=opals, subset='subD', data='D', logical='PM_BMI_CONTINUOUS>=', threshold=25)
#' 
#' # Example 4: get the logarithmic values of the variable 'lab_hdl' and generate a subset with 
#' the first 50 observations of that new vector. If the specified number of row is greater than the total 
#' number of rows in any of the studies the process will stop.
#' ds.assign(opals, 'logHDL', 'log(D$LAB_HDL)')
#' ds.subset(datasources=opals, subset="subLAB_HDL", data="logHDL", rows=c(1:50))
#' 
#' # Example 5: get the variable 'PM_BMI_CONTINUOUS' from the dataframe 'D' and generate a subset bmi
#' vector with bmi values greater than or equal to 25
#' ds.assign(opals, "BMI", "D$PM_BMI_CONTINUOUS")
#' ds.subset(datasources=opals, subset='BMI25plus', data='BMI', logical='>=', threshold=25)
#' 
#' }
#' 
ds.subset <- function(datasources=NULL, subset="subsetObject", data=NULL, complete=FALSE, rows=NULL, cols=NULL, logical=NULL, threshold=NULL){
  
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
  
  # call the internal function that checks the input data is of the same class in all studies.
  typ <- dsbaseclient:::.checkClass(datasources, data)
  
  # the argument 'rows' and 'cols' have precedence over 'logical' and 'threshold'
  if(is.null(rows) & is.null(cols)){
    if(is.null(logical) | is.null(threshold)){
      if(!complete){
        message("None of the subsetting parameters is provided, the sought subset is the same as the original object!")
        stop(" End of process!", call.=FALSE)
      }else{
        cally <- call('subsetDS', dt=data, complt=complete)
        datashield.assign(datasources, subset, cally)
      }
    }else{
      # allow this only for numeric vector
      if(typ == "factor" | typ == "character"){
        stop(" Subsetting on a threshold criteria is allowed only for numeric vectors!", call.=FALSE)
      }
      # get the logical operator and any variable provided with it
      lg <- unlist(strsplit(logical, split=""))
      var2sub <- NULL
      if(lg[length(lg)] == "=" & lg[(length(lg)-1)] == ">" | lg[(length(lg)-1)] == "<" | lg[(length(lg)-1)] == "=" |  lg[(length(lg)-1)] == "!"){
        logical <- paste0(lg[(length(lg)-1)], lg[length(lg)])
        if(length(lg) > 2){ var2sub <- paste(lg[1:(length(lg)-2)], collapse="") }
      }else{
        logical <- lg[length(lg)]
        if(length(lg) > 1) { var2sub <- paste0(lg[1:(length(lg)-1)], collapse="") }
      }
      if(is.null(var2sub)){ # if input is table, logical & threshold apply but no variable name given
        stop("No variable to subset the table on - see example 3 in the documentation of this function.", call.=FALSE)
      }
      # turn the logical operator into the corresponding integer that will be evaluated on the server side.
      logical <- dsbaseclient:::.logical2int(logical)
      cally <- call('subsetDS', dt=data, complt=complete, rs=rows, cs=cols, lg=logical, th=threshold, varname=var2sub)
      datashield.assign(datasources, subset, cally)
    }
  }else{
    # if now value provided for the argument 'rows' consider all the rows in each study dataset
    if(is.null(rows)){
      rows <- c()
      for(i in 1:length(datasources)){
        r <- ds.dim(datasources[i], data)
        rows <- append(rows, r[[1]])
      }
    }else{
      rows <- rep(rows, length(datasources))
    }
    # if now value provided for the argument 'cols' consider all the columns
    if(is.null(cols)){
      cols <- length(unique(unlist(ds.colnames(datasources, data))))
    }
    
    # check if the provided range is not larger than the vector or the table size
    # if not call the server side function and carry out the subsetting
    if(typ == "factor" | typ == "numeric" | typ == "character"){
      # if the size of the requested subset is greater than that of original inform the user and stop the process
      ll <- ds.length(datasources, data, type="split")
      fail <- 0
      for(i in 1:length(datasources)){
        if(length(rows[i]) > ll[[i]]){
          fail <- append(fail, i)        
        }
      }
      if(sum(fail) > 0){
        stop(paste0("The subset you specified is larger than ", data, " in ", paste(stdnames[fail], collapse=", "), "."))
      }else{
        # turn the vector of row indices into a character to pass the parser
        for(i in 1:length(datasources)){
          cally <- call('subsetDS', dt=data, complt=complete, rs=rows[i])
          datashield.assign(datasources[i], subset, cally)
        }
      }
    }else{
      if(typ == "data.frame" | typ == "matrix"){
        # call the internal function that ensure wrond size subset is not requested (i.e. subset size > original table)
        for(i in 1:length(datasources)){
          dsbaseclient:::.subsetHelper(datasources[i], subset, data, rows[i], cols)
        }
        for(i in 1:length(datasources)){
          cally <- call('subsetDS', dt=data, complt=complete, rs=rows, cs=cols)
          datashield.assign(datasources[i], subset, cally)
        }
      }else{
        stop("The object to subset from must be a numeric, character or factor vector or a table (matrix or data.frame).", call.=FALSE)
      }
    }
  }

  # a message so the user knows the function was ran (assign functions are 'silent')
  message("An 'assign' function was ran, no output should be expected on the client side!\n")
  message("If the generated subset dataframe contains only missing values(i.e. NA) this indicates and invalid subset!")

}
