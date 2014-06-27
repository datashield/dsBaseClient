#' 
#' @title Generates valid subset(s) of a dataframe or a factor
#' @description The function takes a categorical variable or a data frame as input and generates 
#' subset(s)  variables or data frames for each category.
#' @details If the input data object is a data frame it is possible to specify  the variables  
#' to subset on. If a subset is not 'valid' all its the values are reported as missing (i.e. NA),
#' the name of the subsets is labelled as '_INVALID'. Subsets are considered invalid if the number
#' of observations it holds are less than the agreed threshold (e.g. 5).
#' @param x a character, the name of the dataframe or the vector to generate subsets from.
#' @param variables a vector of string characters, the name(s) of the variables to subset by.
#' @param subsets the name of the output object, a list that holds the subset objects. If set to NULL
#' the default name of this list is 'subClasses'.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @return a no data are return to the user but messages are printed out.
#' @author Gaye, A.
#' @export
#' @examples {
#'
#' # load the login data
#' data(logindata)
#' 
#' # login and assign some variables to R
#' myvar <- list('DIS_DIAB','PM_BMI_CONTINUOUS','LAB_HDL', 'GENDER')
#' opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#' # Example 1: generate all possible subsets from the table assigne above 
#' ds.subclass(x='D', subsets='subclasses')
#' 
#' # Example 2: subset the table initially assigned by the variable 'GENDER'
#' ds.subclass(x='D', subsets='subtables', variables='GENDER')
#' 
#' # Example 3: generate a new variable 'gender' and split it into two vectors: males and females
#' ds.assign(newobj='gender', toAssign='D$GENDER')
#' ds.subclass(x='gender', subsets='subvectors')
#' 
#' }
#' 
ds.subclass <- function(x=NULL, subsets="subClasses", variables=NULL, datasources=NULL){
  
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
    stop("Please provide the name of the input vector!", call.=FALSE)
  }
  
  # check if the input x is defined in all the studies
  defined <- isDefined(datasources, x)
  
  # call the internal function that checks the input object is of the same class in all studies.
  typ <- checkClass(datasources, x)
  
  # the input object must be a dataframe or a factor
  if(typ != 'data.frame' & typ != 'factor'){
    message(paste0(x, " is of type ", typ, "!"))
    stop("The object to subset from must be of type 'data.frame' or 'factor'.", call.=FALSE)
  }
  
  # number of studies and their names
  numsources <- length(datasources)
  stdnames <- names(datasources)
  
  # check that, for each study,  all the columns of the input table are of 'numeric' type
  if(typ == 'data.frame'){
    dtname <- x
    for(i in 1:numsources){
      cols <- datashield.aggregate(datasources[i], paste0("colnames(", x, ")"))
      dims <- datashield.aggregate(datasources[i], paste0("dim(", x, ")"))
      tracker <-c()
      for(j in 1:dims[[1]][2]){
        cally <- paste0("class(", dtname, "$", cols[[1]][j], ")")
        res <- datashield.aggregate(datasources[i], cally)
        if(res[[1]] != 'factor'){
          tracker <- append(tracker, 0)
        }else{
          tracker <- append(tracker, 0)
        }
      }
      if(sum(tracker) !=0){
        stop("No factor variable found in ", dtname, " in ",  stdnames[i], ".", call.=FALSE)
      }
    }
  }
  
  # call the server side function that does the job
  # get the indices of the columns refered to by their names in the arguments
  if(is.null(variables)){
    cally <- paste0("subclassDS('", x, "')")
  }else{
    cally <- paste0("subclassDS('", x, "', c('",paste(variables,collapse="','"),"'))")
  }
  datashield.assign(datasources, subsets, as.symbol(cally))
  
  # check that the new object has been created and display a message accordingly
  finalcheck <- isAssigned(datasources, subsets)
  
}
