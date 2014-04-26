#'
#' @title  Computes the mean and standard deviation across categories
#' @description This function calculates the mean and SD of a continuous variable for each class 
#' of up to 3 categorical variables.
#' @details The functions splits the input dataset into the single categories and calculates the mean and SD.
#' If the subset table is invalid (contains less than the number of allowed observation) or empty (no observations
#' in that categorie) a missing value is returned for both the mean and the standard deviation.
#' It is important to note that the process of generating the final table(s) can be time consuming particularly if 
#' the subsetting is done across more than one categorical variable and the run-time lengthens if the parameter 'split'
#' is set to 'split' as a table is then produced for each study. It is therefore advisable to run the function only for the
#' studies of the user really interested in but including only those studies in the parameter 'datasources'.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @param dataset the name given to the dataset when it was assigned from opal to R.
#' @param outvar a character vector, the names of the continuous variables
#' @param covar a character vector, the names of up to 3 categorical variables
#' @param type a character which represents the type of analysis to carry out. 
#' If \code{type} is set to 'combine', a pooled table of results is generated.
#' if \code{type} is set to 'split', a table of results is genrated for each study.
#' @return a table or a list of tables that hold the length of the continuous variable(s) and their mean 
#' and standard deviation in each subgroup (subset).
#' @export
#' @author Gaye, A.
#' @examples {
#' 
#' # load that contains the login details
#' data(logindata)
#' 
#' # login and assign the whole dataset on the opal server
#' opals <- datashield.login(logins=logindata,assign=TRUE)
#' 
#' # Example 1: calculate the mean proportion for LAB_HDL across gender categories
#' ds.meanByClass(datasources=opals, dataset='D', outvar='LAB_HDL', covar='GENDER')
#' 
#' # Example 2: calculate the mean proportion for LAB_HDL across gender and bmi categories
#' ds.meanByClass(datasources=opals, dataset='D', outvar=c('LAB_HDL','LAB_TSC'), covar=c('GENDER'))
#' 
#' # Example 3: calculate the mean proportion for LAB_HDL across gender bmi and diabetes status categories
#' ds.meanByClass(datasources=opals, dataset='D', outvar=c('LAB_HDL','LAB_TSC'), covar=c('GENDER','PM_BMI_CATEGORICAL','DIS_DIAB'))
#' 
#' # Example 4: calculate the mean proportion for LAB_HDL across gender categories for each study separately.
#' results <- ds.meanByClass(datasources=opals, dataset='D', outvar='LAB_HDL', covar='GENDER', type='split')
#' 
#' }
#' 
ds.meanByClass <-  function(datasources=NULL, dataset=NULL, outvar=NULL, covar=NULL, type='combine'){
  if(is.null(datasources)){
    message("No valid opal object(s) provided!")
    message("Make sure you are logged in to valid opal server(s).")
    stop(" End of process!", call.=FALSE)
  }else{
    stdnames <- names(datasources)
  }
  
  if(is.null(dataset)){
    message("No input dataset provided")
    message("Check the parameter 'dataset'.")
    stop(" End of process!", call.=FALSE)
  }
  
  if(is.null(outvar)){
    message("No continuous variable provided")
    message("Check the parameter 'outvar'.")
    stop(" End of process!", call.=FALSE)
  }
  
  if(is.null(covar)){
    message("No categorical provided")
    message("You must specify at least one categorical variable.")
    stop(" End of process!", call.=FALSE)
  }
  
  if(length(covar) > 3){
    warning("More than 3 categorical have been specified. Only the first 3 will be considered!")
    covar <- covar[1:3]
  }
  
  # categories in each of the categorical variables
  classes <- vector("list", length(covar))
  for(i in 1:length(covar)){
    classes[[i]] <- unique(unlist(ds.levels(datasources=datasources[1], xvect=paste0(dataset, '$', covar[i]))))
  }
  
  # loop through the datasources a break down the original dataset by the specified categorical variable
  # the names of the subset tables are stored for mean and sd computations
  message("Generating the required subset tables (this may take couple of minutes, please do not interrupt!)")
  subsetnames <- vector("list", length(datasources))
  for(i in 1:length(datasources)){
    message("--", names(datasources)[i])
    datasets <- dataset
    for(j in 1:length(covar)){
      message("  ", covar[j], "...")
      newnames <- dsbaseclient:::.meanByClassHelper1(datasources[i], datasets, covar[j], classes[[j]])
      datasets <- newnames
    }
    subsetnames[[i]] <- datasets
  }  
  names(subsetnames) <- names(datasources)
  

  # a study might have invalid sub-datasets which we cannot get mean and sd from, to idenitfy those
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
    results <- dsbaseclient:::.meanByClassHelper2(datasources, subsetnames, outvar, invalidrecorder)
    return(results)
  }else{
    if(type=='split'){
      results <- dsbaseclient:::.meanByClassHelper3(datasources, subsetnames, outvar, invalidrecorder)
      return(results)
    }else{
      stop('Function argument "type" has to be either "combine" or "split"')
    }
  }
  
}






