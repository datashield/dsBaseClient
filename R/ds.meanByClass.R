#'
#' @title  Computes the mean and standard deviation across categories
#' @description This function calculates the mean and SD of a continuous variable for each class 
#' of up to 3 categorical variables.
#' @details The functions splits the input dataset into subsets (one for each category) and calculates 
#' the mean and SD of the specified numeric variables. It is important to note that the process of 
#' generating the final table(s) can be time consuming particularly if the subsetting is done across 
#' more than one categorical variable and the run-time lengthens if the parameter 'split' is set to 
#' 'split' as a table is then produced for each study. It is therefore advisable to run the function 
#' only for the studies of the user really interested in but including only those studies in the 
#' parameter 'datasources'.
#' @param x a character, the name of the dataset to get the subsets from or a text formula of the 
#' form 'A~B' where A is a single continuous vector and B a single factor vector
#' @param outvar a character vector, the names of the continuous variables
#' @param covar a character vector, the names of up to 3 categorical variables
#' @param type a character which represents the type of analysis to carry out. If \code{type} is set to 
#' 'combine', a pooled table of results is generated. If \code{type} is set to 'split', a table of results 
#' is genrated for each study.
#' @param datasources a list of opal object(s) obtained after login in to opal servers; these objects hold 
#' also the data assign to R, as \code{dataframe}, from opal datasources.
#' @return a table or a list of tables that hold the length of the numeric variable(s) and their mean 
#' and standard deviation in each subgroup (subset).
#' @export
#' @author Gaye, A.
#' @seealso \link{ds.subsetByClass} to subset by the classes of factor vector(s).
#' @seealso \link{ds.subset} to subset by complete cases (i.e. removing missing values), threshold, columns and rows.
#' @examples 
#' \dontrun{
#' 
#'   # load that contains the login details
#'   data(logindata)
#' 
#'   # Example 1: calculate the pooled mean proportion for LAB_HDL across GENDER categories where
#'   # both vectors are in a tabe structure "D"
#'   # login and assign LAB_HDL and GENDER to a table "D"
#'   opals <- datashield.login(logins=logindata,assign=TRUE, variables=list('LAB_HDL', 'GENDER'))
#'   ds.meanByClass(x='D$LAB_HDL~D$GENDER')
#'   
#'   # Example 2: calculate the mean proportion for LAB_HDL across GENDER categories where both
#'   # vectors are 'loose' (i.e. not in a table)
#'   # assign both LAB_HDL and GENDER to vectors not held in a table
#'   ds.assign("D$LAB_HDL", "ldl")
#'   ds.assign("D$GENDER", "sex")
#'   ds.meanByClass(x='ldl~sex')
#'   datashield.logout(opals)
#'   
#'   # Example 3: calculate the mean proportion for LAB_HDL across gender, bmi and diabetes status
#'   # categories login and assign all the variables stored on opal
#'   opals <- datashield.login(logins=logindata,assign=TRUE)
#'   ds.meanByClass(x='D', outvar=c('LAB_HDL','LAB_TSC'), covar=c('GENDER','PM_BMI_CATEGORICAL',
#'                  'DIS_DIAB'))
#' 
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(opals)
#' 
#' }
#' 
ds.meanByClass <-  function(x=NULL, outvar=NULL, covar=NULL, type='combine', datasources=NULL){
  
  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }
  
  # check if the user specified a formula to run the process for two loose vector or if the vectors are 
  # in a table structure (data frame or matrix) and call the relevant function accordingly
  if(is.null(x)){
    stop("Please provide the name data frame or matrix or a formula of the form 'A~B' where A is a continuous vector and B a factor vector!", call.=FALSE)
  }else{
    obj <- unlist(strsplit(x, split='~'))
    if(length(obj)==2){
      # check if the input variables are defined in all the studies
      defined <- isDefined(datasources, obj[1])
      defined <- isDefined(datasources, obj[2])
      typ <- checkClass(datasources, obj[1])
      if(!("numeric" %in% typ) & !("integer" %in% typ)){
        stop("The first element in the formula must be of type numeric or integer!", call.=FALSE)
      }
      typ <- checkClass(datasources, obj[2])
      if(!("factor" %in% typ)){
        stop("The second element in the formula must be of type factor!", call.=FALSE)
      }
      output <- meanByClassHelper0a(obj[1], obj[2], type, datasources)
      return(output)
    }else{
      if(length(obj)==1){
        defined <- isDefined(datasources, x)
        typ <- checkClass(datasources, x)
        if(!("data.frame" %in% typ) & !("matrix" %in% typ)){stop("x must be the name of a data frame or a matrix or a formula of the form 'A~B' where A is a continuous vector and B a factor vector!", call.=FALSE)}
        output <- meanByClassHelper0b(x, outvar, covar, type, datasources)
        return(output)
      }else{
        stop("x must be the name of a data frame or a matrix or a formula of the form 'A~B' where A is a continuous vector and B a factor vector!", call.=FALSE)
      }
    }
  }
  
}

