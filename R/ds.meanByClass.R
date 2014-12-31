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
#' @param x a character, the name of the dataset to get the subsets from.
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
#' @seealso \link{ds.subsetByClass}
#' @examples {
#' 
#'   # load that contains the login details
#'   data(logindata)
#' 
#'   # login and assign the whole dataset on the opal server
#'   opals <- datashield.login(logins=logindata,assign=TRUE)
#' 
#'   # Example 1: calculate the mean proportion for LAB_HDL across gender categories
#'   ds.meanByClass(x='D', outvar='LAB_HDL', covar='GENDER')
#' 
#'   # Example 2: calculate the mean proportion for LAB_HDL across gender and bmi categories
#'   ds.meanByClass(x='D', outvar=c('LAB_HDL','LAB_TSC'), covar=c('GENDER'))
#' 
#'   # Example 3: calculate the mean proportion for LAB_HDL across gender, bmi and diabetes status categories
#'   ds.meanByClass(x='D', outvar=c('LAB_HDL','LAB_TSC'), covar=c('GENDER','PM_BMI_CATEGORICAL','DIS_DIAB'))
#' 
#'   # Example 4: calculate the mean proportion for LAB_HDL across gender categories for each study separately.
#'   ds.meanByClass(x='D', outvar='LAB_HDL', covar='GENDER', type='split')
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
  
  if(is.null(x)){
    stop("Please provide the name of the input vector!", call.=FALSE)
  }
  
  # check if the input x is defined in all the studies
  defined <- isDefined(datasources, x)
  
  # call the internal function that checks the input object is of the same class in all studies.
  typ <- checkClass(datasources, x)
  
  # the input object must be a dataframe
  if(typ != 'data.frame'){
    stop(paste0(x, "must be a 'data.frame'."), call.=FALSE)
  }
  
  if(is.null(outvar)){
    stop(" Please specify at least 1 continuous variables - see parameter 'outvar'!", call.=FALSE)
  }
  
  if(is.null(covar)){
    stop(" Please specify at 1 or up to 3 categorical variables (factors) - see parameter 'covar'!", call.=FALSE)
  }
  
  if(length(covar) > 3){
    stop("More than 3 categorical variables specified! - see parameter 'covar'.", call.=FALSE)
  }
  
  # categories in each of the categorical variables
  classes <- vector("list", length(covar))
  for(i in 1:length(covar)){
    cally <- paste0("levels(",paste0(x, '$', covar[i]), ")")
    classes[[i]] <- datashield.aggregate(datasources, as.symbol(cally))
  }
  
  # loop through the datasources and break down the original dataset by the specified categorical variable
  # the names of the subset tables are stored for mean and sd computations
  message("Generating the required subset tables (this may take couple of minutes, please do not interrupt!)")
  subsetnames <- vector("list", length(datasources))
  for(i in 1:length(datasources)){
    message("--", names(datasources)[i])
    datasets <- x
    for(j in 1:length(covar)){
      message("  ", covar[j], "...")
      newnames <- meanByClassHelper1(datasources[i], datasets, covar[j], classes[[j]])
      datasets <- newnames
    }
    subsetnames[[i]] <- datasets
  }  
  names(subsetnames) <- names(datasources)

  # a study might have invalid sub-datasets which we cannot get mean and sd from, to identify those
  # we loop by categories, if a study has invalid table (i.e. table with NAs only) we exclude it
  # for that category when calculating the mean and sd values for that category
  invalidrecorder <- vector("list", length(datasources))
  for(i in 1:length(datasources)){
    for(j in 1:length(subsetnames[[i]])){
      check1 <- which(unlist(strsplit(subsetnames[[i]][j],"_")) == "INVALID")
      check2 <- which(unlist(strsplit(subsetnames[[i]][j],"_")) == "EMPTY")
      if(length(check1) > 0 | length(check2 > 0)){ 
        invalidrecorder[[i]] <- append(invalidrecorder[[i]], 1) 
      }else{
        invalidrecorder[[i]] <- append(invalidrecorder[[i]], 0) 
      }
    }          
  }
  
  # compute the length, mean and standard deviation for each 'outvar'
  if(type=='combine'){
    results <- meanByClassHelper2(datasources, subsetnames, outvar, invalidrecorder)
    return(results)
  }else{
    if(type=='split'){
      results <- meanByClassHelper3(datasources, subsetnames, outvar, invalidrecorder)
      return(results)
    }else{
      stop('Function argument "type" has to be either "combine" or "split"')
    }
  }
  
}






