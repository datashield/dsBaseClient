#' 
#' @title Generates an expanded version of a dataset that contains survival data
#' @description This function is meant to be used as part of a piecewise regression analysis.
#' @details It splits the survial interval time of subjects into sub-intervals and reports the failure 
#' status of the subjects at each sub-interval. Each of those sub-interval is given an id e.g. if the overall
#' interval of a subject is split into 4 sub-interval, those sub-intervals have ids 1, 2, 3 and 4; so this is 
#' basically the count of periods for each subject. The interval ids are held in a column named "TIMEID". 
#' The entry and exit times in the input table are used to compute the total survival time. 
#' By default all the covariates in the input table are included in the expanded output table but it is 
#' preferable to indicate the names of the covariates to be included via the argument 'variables'.
#' @param data a character, the name of the table that holds the original data, this is the data to be expanded.
#' @param intervalWidth, a numeric vector which gives the chosen width of the intervals ('pieces'). 
#' This can be one value (in which case all the intervals have same width) or several different values.
#' If no value(s) are provided a single default value is used. That default value is the set to be the 
#' 1/10th of the mean of the exit time values across all the studies.
#' @param idCol a character the name of the column that holds the individual IDs of the subjects.
#' @param entryCol a character, the name of the column that holds the entry times (i.e. start of follow up).
#' If no name is provided the default is to set all the entry times to 0 in a column named "STARTTIME".
#' A message is then printed to alert the user as this has serious consequences if the actual entry times are 
#' not 0 for all the subjects. 
#' @param exitCol a character, the name of the column that holds the exit times (i.e. end of follow up).
#' @param statusCol a character, the name of the column that holds the 'failure' status of each subject, 
#' tells whether or not a subject has been censored.
#' @param variables a character vector, the column names of the variables (covariates) to include in the 
#' final expanded table. The input table might have a large number of covariates and if only some of those
#' variables are relevant for the sought analysis it make sense to only include those. By default (i.e. if
#' no variables are indicated) all the covariates in the inout table are included and this will lengthen the
#' run time of the function. 
#' @param newobj the name of the output expanded table. By default the name is the name of the input table with 
#' the suffixe "_expanded".
#' @param datasources a list of opal object(s) obtained after login to opal servers;
#' these objects also hold the data assigned to R, as a \code{data frame}, from opal datasources
#' @return a dataframe, an expanded version of the input table.
#' @author Gaye, A.
#' @seealso \code{ds.glm} for genralized linear models
#' @seealso \code{ds.gee} for generalized estimating equation models
#' @export
#' @examples {
#' 
#'   # load the file that contains the login details
#'   data(survivalLoginData)
#' 
#'   # login and assign all the variables to R
#'   opals <- datashield.login(logins=survivalLoginData,assign=TRUE)
#' 
#'   # this example shows how to run survival analysis in H-DataSHIELD using the 'piecewise exponential regression' method
#' 
#'   # let us display the names of the variables in the original table (the table we assigned above and which by default is named 'D')
#'   ds.colnames('D')
#' 
#'   # specify some baseline hazard profile (i.e. the width of the intervals to be used)
#'   bh <- c(2,1,3,0.5,1.5,2)
#' 
#'   # expand the original table (e.g the survial time of each individual is split into 'pieces' equal to the intervals specified above
#'   # we use the function 'ds.lexis' which expands the original table and saves the expanded table on the server site.
#'   # we set the parameter 'variables' to NULL (default) which means include all the covariates in the expanded table - It is preferable
#'   # to indicate the variables to include if you have many variables and wants to use only a subset of those.
#'   ds.lexis(data='D', intervalWidth=bh, idCol="ID", entryCol="STARTTIME", exitCol="ENDTIME", statusCol="CENS")
#' 
#'   # let us display the names of variables in the expanded table (by default it is the name of the priginal table followed by '_expanded')
#'   ds.colnames('D_expanded')
#'
#'   # Now fit a GLM with a poisson model
#'   # there is a direct relationship between the poisson model with a log-time offset and the exponential model so we can 
#'   # use glm to fit a poisson model and include a factor for the time intervals ('TIMEID') to have different rates.
#'   # The vector 'SURVIVALTIME' (the time elapsed between start of follow up failure/censoring) and the vector 'TIMEID' 
#'   # which allows for different rates are generated when the initial table got expanded via the function 'ds.lxus'. 
#'   # In the below model the log of the survival time is used as an offset (some known information to be included in the model).
#'   
#'   # generate a vector of log survival time values
#'   ds.assign(toAssign='log(D_expanded$SURVIVALTIME)', newobj='logSurvival')
#'   
#'   # Fit the GLM - the outcome is failure status
#'   ds.glm(formula='CENS~1+TIMEID+AGE.60+GENDER+NOISE.56+PM10.16', data='D_expanded', family='poisson', offset='logSurvival')
#'  
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(opals) 
#' }
#'
ds.lexis <- function(data=NULL, intervalWidth=NULL, idCol=NULL, entryCol=NULL, exitCol=NULL, statusCol=NULL, variables=NULL, newobj=NULL, datasources=NULL){
  
  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }
  
  # check if user have provided a the name of the input dataset
  if(is.null(data)){
    stop("Please provide the name of the dataset to expand!", call.=FALSE)
  }
  
  # check if user have provided the name of the column that holds the subject ids
  if(is.null(idCol)){
    stop("Please provide the name of the column that holds the subject IDs!", call.=FALSE)
  }
  
  # check if user have provided the name of the column that holds failure information
  if(is.null(statusCol)){
    stop("Please provide the name of the column that holds 'failure' information!", call.=FALSE)
  }
  
  # check if user have provided the name of the column that holds exit times 
  if(is.null(exitCol)){
    stop("Please provide the name of the column that holds the exit times (i.e. end of follow up time)!", call.=FALSE)
  }
  
  # if no value provided for 'intervalWidth' generate one
  if(is.null(intervalWidth)){
    intervalWidth <- lexisHelper1(datasources, paste0(data,"$",exitCol))
  }
  
  if(is.null(newobj)){
    newobj <- paste0(data,"_expanded")
  }
  
  # call the server side function
  cally <- call("lexisDS", data, intervalWidth, idCol, entryCol, exitCol, statusCol, variables)
  datashield.assign(datasources, newobj, cally)
  
  # check that the new object has been created if and display a message accordingly
  finalcheck <- isAssigned(datasources, newobj)  
  
}