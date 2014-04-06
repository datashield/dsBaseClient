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
  dtsources <- datasources
  # subset the first categorical variable and remove the subset tables from the list where they are stored
  ds.subclass(datasources=dtsources, subsets='sublist', data=dataset, variables=covar[1])
  categories <- unique(unlist(ds.levels(datasources=dtsources, xvect=paste0(dataset, '$', covar[1]))))
  nextTables2subset <- dsbaseclient:::.meanByClassHelper1(opals, dataset, 'sublist', covar[1], categories)
  
  # if there are more than one covariate loop and subset repeatedly the dataset 
  # on the categories of each of the covariates
  if(length(covar) > 1){
    for(s in 2:length(covar)){
      # generate subsets from the datasets obtained from the previous subsetting using the second categorical variable
      tempholder <- c()
      cls <- unique(unlist(ds.levels(datasources=dtsources, xvect=paste0(dataset, '$', covar[s]))))
      for(i in 1:length(nextTables2subset)){
        listname <- paste0(nextTables2subset[i], ".",covar[s])
        tablename <- nextTables2subset[i]
        ds.subclass(datasources=dtsources, subsets=listname, data=tablename, variables=covar[s])
        xnames <- dsbaseclient:::.meanByClassHelper1(opals, tablename, listname, covar[s], cls)
        tempholder <- append(tempholder, xnames)
      }
      # tables to subset for the third variables or to use for the mean and SD calculations if only 2 categorical variables were given
      nextTables2subset <- tempholder
    }
  }
  
  # compute the length, mean and standard deviation for each 'outvar'
  message('Final results table(s) are being generated, this might take couple of minutes if more than one categorical variable was specified.')
  if(type=='combine'){
    results <- dsbaseclient:::.meanByClassHelper2(dtsources, nextTables2subset, outvar)
    return(results)
  }else{
    if(type=='split'){
      results <- dsbaseclient:::.meanByClassHelper3(dtsources, nextTables2subset, outvar)
      return(results)
    }else{
      stop('Function argument "type" has to be either "combine" or "split"')
    }
  }
  
}






