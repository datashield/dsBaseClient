#'
#' @title Create an object of class Lexis to represent follow-up in multiple states on multiple time scales.
#' @description This function takes a data frame containing survival data and expands it by converting records at
#' the level of individual subjects (survival time, censoring status, IDs and other variables) into
#' multiple records over a series of pre-defined time intervals. For each survival interval the
#' expanded data frame contains variables denoting the surival time and the censoring status in that
#' specific interval, a unique ID for every time interval and carries copies of other IDs and
#' variables. 
#' 
#' This function is particularly meant to be used in preparing data for a piecewise
#' regression analysis (PAR). Although the time intervals have to be pre-specified and are
#' arbitrary, even a vaguely reasonable set of time intervals will give results very similar to a
#' Cox regression analysis. The key issue is to choose survival intervals such that the baseline
#' hazard (risk of death/disease/failure) within each interval is reasonably constant while the
#' baseline hazard can vary freely between intervals. Even if the choice of intervals is very poor
#' the ultimate results are typically qualitatively similar to Cox regression. Increasing the
#' number of intervals will inevitably improve the approximation to the true baseline hazard, but
#' the addition of many more unecessary time intervals slows the analysis and can become disclosive
#' and yet will not improve the fit of the model. 
#' 
#' If the number of failures in one or more time
#' periods is a given study is less than the specified disclosure filter determining minimum
#' acceptable cell size in a table (\code{nfilter.tab}) 
#' then the expanded data frame is not created in that study, and a study-side message 
#' to this effect is made available in that study via \code{ds.message()} function.
#' @details The function \code{ds.lexis} splits the survival interval time of subjects into pre-specified
#' sub-intervals that are each assumed to encompass a constant base-line hazard which means a
#' constant instantaneous risk of death). In the expanded dataset a row is included for every
#' interval in which a given individual is followed - regardless how short or long that period may
#' be. Each row includes:\cr
#' (1) a variable (CENSOR) indicating failure status for a particular
#' interval in that interval also known as censoring status (failed = 1, died, relapsed, developed a
#' disease etc, 0= e.g. lost-to-follow-up or passed right through the interval without failing);\cr
#' (2) an exposure-time variable (SURVTIME) indicating the duration of exposure-to-risk-of-failure
#' the corresponding individual experienced in that interval before he/she failed or was censored).
#' 
#' To illustrate, an individual who survives through 5 such intervals and then dies/fails in the
#' 6th interval will be allocated a 0 value for the failure status/censoring variable in the first
#' five intervals and a 1 value in the 6th, while the exposure-time variable will be equal to the
#' total length of the relevant interval in each of the first five intervals, and the additional
#' length of time they survived in the sixth interval before they failed or were censored. If they
#' survive through the first interval and they are censored in the second interval, the
#' failure-status variable will take the value 0 in both intervals.\cr
#' (3) The expanded data set also
#' includes a unique ID (UID.expanded)in a form such as 77.13 which identifies that row of the
#' dataset as relating to the 77th individual in the input data set (in whatever order they have
#' been placed by the data repository 
#' [which is often different to the original numeric order of the IDs that were
#' actually specified to the data repository]) and his/her experience 
#' (exposure-time and failure status)in the
#' 14th interval. Note that .N indicates the (N+1)th interval because interval 1 has no
#' suffix.\cr
#' (4) In addition to UID.expanded, the expanded dataframe also includes a simpler variable
#' IDSEQ which is simply the first part of UID.expanded (before the '.'). The value of this
#' variable is repeated in every row to which the corresponding individual contributes data (i.e.
#' to every row corresponding to an interval in which that individual was followed) (5) Finally,
#' the expanded dataset contains any other variables pertaining to each individual that the user
#' would like to  carry forwarded to a survival analysis based on the expanded data. Typically,
#' this will include the original ID as specified to the data repository, the total survival time (equivalent to
#' the sum of the exposure times across all intervals) and the ultimate failure-status in the final
#' interval to which they were exposed.  The value of each of these variables is also repeated in
#' every row corresponding to an interval in which that individual was followed. The clientside
#' function ds.lexis calls three server side functions. First lexisDS1 which is an aggregate
#' function. This identifies the maximum survival time in each study (with a positive random value
#' added to prevent disclosure). When these are all returned to the clientside, the maximum of
#' these maxima us selected and this ensures that the end of the final exposure period will always
#' include all of the events in all studies. The value of this maximum maximum is returned as part
#' of the output of ds.lexis - REMEMBER IT INCLUDES A RANDOM ADDITION SO IT WILL ALWAYS BE LARGER
#' THAN THE ACTUAL LARGEST SURVIVAL TIME BUT THIS DOESN'T MATTER TOO MUCH THOUGH IF IT IS LARGE
#' RELATIVE TO THE REAL LENGTH OF THE FINAL SURVIVAL PERIOD, IT WILL DISTORT (REDUCE) THE ESTIMATE
#' OF THE BASELINE HAZARD IN THE FINAL TIME PERIOD. IN THE UNLIKELY EVENT THAT THIS IS A PROBLEM
#' FOR ANYONE, WE COULD EXPLORE A WORK ROUND IN A LATER VERSION OF DataSHIELD. Second, lexisDS2
#' undertakes the actual expansion to produce the new dataframe. The function lexisDS2, which is an
#' aggregate function, also checks the arguments to identify disclosure risks. This includes any
#' attempts to send illegal character strings to the serverside as part of the intervalWidth
#' argument. It also checks that the total length of the intervalWidth vector (effectively the
#' total number of intervals specified) does not exceed nfilter.glm*length of vectors in the
#' collapsed dataframe (before expansion to produce expandDF. This is because intervalWidth defines
#' a numeric vector (completely determined by the user) which might be used to create and define
#' subsets if the vector you defined was the same length as the primary data vectors in the model.
#' In addition lexisDS2 checks the number of failures in each time interval and if one or more
#' intervals in a study contain fewer than the value of nfilter.tab (the minimum valid non-zero
#' cell count in a table) set by the server administrator for that study, the test will be failed
#' for the relevant server. If any of these tests are failed, creation of the expanded dataframe
#' will be blocked and an explanatory error message will be stored on each server. These messages
#' can then be read using the command: ds.message("messageobj"). Third, the assign function
#' lexisDS3 simplifies the final output so that the object specified by the \code{expandDF}
#' argument is the actual dataframe rather than a table within a list.
#' 
#' In \code{intervalWidth} argument if the total sum of the duration across all intervals is less 
#' than the maximum follow-up of any individual in
#' any contributing study, a final interval will be added by \code{ds.lexis} extending from the end of the
#' last interval specified to the maximum follow-up time. If a single numeric value is specified
#' rather than a vector, \code{ds.lexis} will keep adding intervals of the length specified until the
#' maximum follow-up time in any single study is exceeded. This argument is subject to a number of
#' disclosure checks. 
#' 
#' \code{idCol} argument must be a numeric or character. Note that when a particular variable is
#' identified as being the main ID to the data repository when the data are first transferred 
#' to the data repository (i.e. before
#' DataSHIELD is used), that ID often ends up being of class character and will then be sorted in
#' alphabetic order (treating each digit as a character) rather than numeric. 
#' For example, containing the sequential IDs 1-1000, the order of the IDs will be:\cr
#' 1,10,100,101,102,103,104,105,106,107,108,109,11 ...\cr
#' In an alphabetic listing: NOT to the expected order:\cr
#' 1,2,3,4,5,6,7,8,9,10,11,12,13 ...\cr
#' 
#' This alphabetic order or the ID listing will then carry forward to the
#' expanded dataset. But the nature and order of the original ID 
#' variable held in \code{idCol} doesn't
#' matter to \code{ds.lexis}. Provided every individual appears only once 
#' in the original data set (before expansion) the order does not matter because 
#' \code{ds.lexis} works on its own unique numeric vector
#' that is allocated from \code{1:M} (where there are \code{M} individuals) 
#' in whatever order they appear in the original dataset. 
#' 
#' in \code{entryCol} argument rather than using a total survival time variable to identify the
#' intervals to which any given individual is exposed, \code{ds.lexis} 
#' requires an initial entry time and a final exit time. If the data you wish to expand 
#' contain only a total survival time variable
#' and every individual starts follow-up at time 0, the entry times should all
#' be specified as zero, and the exit times as the total survival time. 
#' So, \code{entryCol} should either be the name of the column 
#' holding the entry time of each individual, or else if no \code{entryCol} is
#' specified it will be defaulted to zero anyway and put into a variable 
#' called \code{starttime} in the expanded data set.
#' 
#' In \code{exitCol} argument if the entry times (\code{entryCol}) are set, 
#' or defaulted, to zero, the \code{exitCol} variable should contain the total survival times.
#' 
#' If \code{variables} argument is not set (is
#' null) but the \code{data} argument is set, the expanded data 
#' set will contain all variables in the data frame identified by the \code{data} argument. 
#' If neither the \code{data} or 
#' \code{variables} arguments are set, the expanded data set will only include the ID, 
#' exposure time and failure/censoring status
#' variables which may still be useful for plotting survival data once these become available.
#' 
#' 
#' 
#' Server functions called: \code{lexisDS1}, \code{lexisDS2} and \code{lexisDS3} 
#' @param data a character string specifying the name of a data frame containing the
#' survival data to be expanded.
#' @param intervalWidth a numeric vector specifying the length of each interval.
#' For more information see \strong{Details}. 
#' @param idCol a character string denoting the column name that holds the individual IDs
#' of the subjects. For more information see \strong{Details}. 
#' @param entryCol a character string denoting the column name that holds the entry times
#' (i.e. start of follow up). For more information see \strong{Details}.
#' @param exitCol a character string denoting the column name that holds the exit times
#' (i.e. end of follow up). For more information see \strong{Details}.
#' @param statusCol a character string denoting the column name that holds the
#' failure/censoring status of each subject. For more information see \strong{Details}.
#' @param variables a vector of character strings denoting the column names of additional
#' variables to include in the final expanded table. For more information see \strong{Details}.
#' @param expandDF a character string denoting the name of the new data frame containing the
#' expanded data set. Default \code{lexis.newobj}. 
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login. 
#' If the \code{datasources} argument is not specified 
#' the default set of connections will be used: see \code{\link{datashield.connections_default}}.
#' @return \code{ds.lexis} returns to the server-side a data frame for each study with 
#' the expanded version of the input table.
#' 
#' @author DataSHIELD Development Team
#' @seealso \code{\link{ds.glm}} for genralized linear models. 
#' @export
#' @examples
#' \dontrun{
#' 
#'  ## Version 6, for version 5 see Wiki
#'   # Connecting to the Opal servers
#'   
#'   require('DSI')
#'   require('DSOpal')
#'   require('dsBaseClient')
#'   
#'   # Example 1: Fitting GLM for survival analysis
#'   # For this analysis we need to load survival data from the server 
#'   
#'   builder <- DSI::newDSLoginBuilder()
#'   builder$append(server = "study1", 
#'                  url = "http://192.168.56.100:8080/", 
#'                  user = "administrator", password = "datashield_test&", 
#'                  table = "SURVIVAL.EXPAND_NO_MISSING1", driver = "OpalDriver")
#'   builder$append(server = "study2", 
#'                  url = "http://192.168.56.100:8080/", 
#'                  user = "administrator", password = "datashield_test&", 
#'                  table = "SURVIVAL.EXPAND_NO_MISSING2", driver = "OpalDriver")
#'   builder$append(server = "study3",
#'                  url = "http://192.168.56.100:8080/", 
#'                  user = "administrator", password = "datashield_test&", 
#'                  table = "SURVIVAL.EXPAND_NO_MISSING3", driver = "OpalDriver")
#'   logindata <- builder$build()
#'   
#'   # Log onto the remote Opal training servers
#'   connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D") 
#'   
#'   #Example 1: Create the expanded data frame. 
#'   #The survival time intervals are to be 0<t<=2.5; 2.5<t<=5.0, 5.0<t<=7.5, 
#'   #up to the final interval of duration 2.5
#'   #that includes the maximum survival time. 
#' 
#'   ds.lexis(data = "D", 
#'            intervalWidth = 2.5,
#'            idCol = "D$id",
#'            entryCol = "D$starttime",
#'            exitCol = "D$endtime",
#'            statusCol = "D$cens",
#'            expandDF = "EM.new",
#'            datasources = connections)
#'            
#'   #Confirm that the expanded data frame has been ceated
#'   ds.ls(datasources = connections) 

#'   #Example 2: Create the expanded data frame. 
#'   #The survival time intervals are to be 0<t<=1; 1<t<=2.0, 2.0<t<=5.0, 5.0<t<=11.0,
#'   
#'   ds.lexis(data = "D",
#'            intervalWidth = c(1,1,3,6), 
#'            idCol = "D$id",
#'            entryCol = "D$starttime", 
#'            exitCol = "D$endtime", 
#'            statusCol = "D$cens",
#'            expandDF = "EM.new2",
#'            datasources = connections)
#'            
#'   #Confirm expanded dataframe created
#'   ds.ls(datasources = connections) 
#' }
#'
ds.lexis<-function(data=NULL, intervalWidth=NULL, idCol=NULL, entryCol=NULL, exitCol=NULL, statusCol=NULL, variables=NULL, expandDF=NULL, datasources=NULL){

  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
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


  # if no valid value provided for 'intervalWidth' instruct user to specify one
  if(is.null(intervalWidth)||is.na(intervalWidth)||intervalWidth==0){
    stop("Please provide a (non-zero) single numeric value or vector to identify the survival time intervals", call.=FALSE)
  }

  # if no value spcified for output (expanded) data set, then specify a default
  if(is.null(expandDF)){
    expandDF <- "lexis.newobj"
  }

#FIRST CALL TO SERVER SIDE TO IDENTIFY THE MAXIMUM FOLLOW UP TIME IN ANY
#SOURCE. THE MAXIMUM I EACH SOURCE IS MASKED BY A RANDOM POSITIVE INCREMENT
  calltext1 <- call("lexisDS1", exitCol)

  maxtime<-DSI::datashield.aggregate(datasources, calltext1)

  num.studies<-length(datasources)

  for(ss in 1:num.studies){
#  print(maxtime[ss][[1]]$max.time)

    if(is.null(maxtime[ss][[1]]$max.time)){
    return(list(maxtime=maxtime))
	}
  }

  nummax<-length(maxtime)

  temp1<-rep(NA,nummax)

  for(j in 1:nummax){
	temp1[j]<-unlist(maxtime[[j]][1])
  }

#IDENTIFY MAXIMUM OF THE MAXIMUM FOLLOW-UP TIMES
  maxmaxtime<-max(temp1)

intervalWidth.transmit<-paste0(as.character(intervalWidth),collapse=",")

#SECOND CALL TO SERVER SIDE USES maxmaxtime AND intervalWidth TO SET
#FOLLOW-UP TIME BREAKS IN EACH STUDY (ALL THE SAME)
  # call the main server side function
  calltext2 <- call("lexisDS2", data, intervalWidth.transmit, maxmaxtime, idCol, entryCol, exitCol, statusCol, variables)
  DSI::datashield.assign(datasources, "messageobj", calltext2)

  calltext3<- call("lexisDS3")
  DSI::datashield.assign(datasources, expandDF, calltext3)


#RETURN COMPLETION INFORMATION TO CLIENT SIDE
  Note1<-"END OF LAST FOLLOW-UP PERIOD SET (RANDOMLY) AT maxmaxtime:"
  Note2<-"ASSIGN FUNCTION COMPLETED - USE ds.ls() TO CONFIRM"
  Note3<-"IF FUNCTION FAILED ON ONE OR MORE STUDIES WITHOUT EXPLANATION, TYPE [PRECISELY] THE COMMAND:"
  Note4<-"ds.message('messageobj') FOR MORE ERROR MESSAGES"
  out.obj<-list(Note1=Note1,maxmaxtime=maxmaxtime,Note2=Note2,Note3=Note3,Note4=Note4)
  return(out.obj)
}
#ds.lexis
