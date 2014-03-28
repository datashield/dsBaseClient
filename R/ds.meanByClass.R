#'
#' @title  Computes the mean and standard deviation across categories
#' @description This function calculates the mean and SD of a continuous variable for each class 
#' of up to 3 categorical variables.
#' @details The functions splits the input dataset into the single categories and calculates the mean and SD.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @param dataset the name given to the dataset when it was assigned from opal to R.
#' @param outvar a character vector, the names of the continuous variables
#' @param covar a character vector, the names of up to 3 categorical variables
#' @return a table that holds length of the continuous variables and their mean and standard error for each class 
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
#' }
#' 
ds.meanByClass <-  function(datasources=NULL, dataset=NULL, outvar=NULL, covar=NULL){
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
  
  # subset the first categorical variable and remove the subset tables from the list where they are stored
  ds.subclass(datasources=datasources, subsets='sublist', data=dataset, variables=covar[1])
  categories <- unique(unlist(ds.levels(datasources=opals, xvect=paste0(dataset, '$', covar[1]))))
  nextTables2subset <- dsbaseclient:::.meanByClassHelper(opals, dataset, 'sublist', covar[1], categories)
  
  # if there are more than one covariate loop and subset repeatedly the dataset 
  # on the categories of each of the covariates
  if(length(covar) > 1){
    for(s in 2:length(covar)){
      # generate subsets from the datasets obtained from the previous subsetting using the second categorical variable
      tempholder <- c()
      cls <- unique(unlist(ds.levels(datasources=opals, xvect=paste0(dataset, '$', covar[s]))))
      for(i in 1:length(nextTables2subset)){
        listname <- paste0(nextTables2subset[i], ".",covar[s])
        tablename <- nextTables2subset[i]
        ds.subclass(datasources=opals, subsets=listname, data=tablename, variables=covar[s])
        xnames <- dsbaseclient:::.meanByClassHelper(opals, tablename, listname, covar[s], cls)
        tempholder <- append(tempholder, xnames)
      }
      # tables to subset for the third variables or to use for the mean and SD calculations if only 2 categorical variables were given
      #categories <- unique(unlist(ds.levels(datasources=opals, xvect=paste0(dataset, '$', covar[s-1]))))
      nextTables2subset <- tempholder
    }
  }
  
  # now get the mean and SD for the continuous variables in each of tthe subset tables
  finaltable <- matrix(numeric(0), ncol=length(nextTables2subset))
  finalrows <- c()
  for(z in 1:length(outvar)){
    # set an empty matrix to hold the results
    outable <- matrix(numeric(0), nrow=2, ncol=length(nextTables2subset))
    xrows <- c(paste0(outvar[z],'(length)'), paste0(outvar[z],'(mean&sd)'))
    for(i in 1:length(nextTables2subset)){
      ll <- unlist(ds.length(datasources, paste0(nextTables2subset[i],'$',outvar[z])))
      mm <- round(unlist(ds.mean(datasources, paste0(nextTables2subset[i],'$',outvar[z]))),2)
      sd <- round(unlist(ds.var(datasources, paste0(nextTables2subset[i],'$',outvar[z]))),2)
      mean.sd <- paste0(mm, '(', round(sqrt(sd),2), ')')
      entries <- c(ll, mean.sd)
      for(j in 1:2){
        outable[j,i] <-  entries[j]
      }
    }
    finalrows <- append(finalrows, xrows)
    finaltable <- rbind(finaltable, outable)
  }
    
  # specify the name of the rows and the columns
  colnames(finaltable) <- nextTables2subset
  rownames (finaltable) <- finalrows
  
  return(finaltable)
  
}






