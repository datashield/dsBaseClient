#' 
#' @title ds.dataFrameFill calling dataFrameFillDS
#' @description Adds extra columns with missing values in a dataframe one for each variable is not
#' included in the dataframe but is included in the relevant datafram of another datasource.
#' @details This function checks if the input data frames have the same variables (i.e. the same
#' column names) in all of the used studies. When a study does not have some of the variables, the
#' function generates those variables as vectors of missing values and combines them as columns to
#' the input data frame. Then, the "complete" in terms of the columns dataframe is saved in each
#' server with a name specified by the argument \code{newobj}. 
#' @param df.name a character string representing the name of the input data frame that will be
#' filled with extra columns with missing values if a number of variables is missing from it
#' compared to the data frames of the other studies used in the analysis.
#' @param newobj a character string providing a name for the output data frame which defaults to
#' the name of the input data frame with the suffix "_filled" if no name is specified.
#' @param datasources specifies the particular opal objects to use. If the \code{datasources}
#' argument is not specified the default set of opals will be used. The default opals
#' are called default.opals and the default can be set using the function ds.setDefaultOpals.
#' @return The object specified by the \code{newobj} argument which is written to the serverside.
#' In addition, two validity messages are returned indicating whether the \code{newobj} has been
#' created in each data source and if so whether it is in a valid form.
#' @author Demetris Avraam for DataSHIELD Development Team
#' @export
#' 
ds.dataFrameFill <- function(df.name=NULL, newobj=NULL, datasources=NULL){
  
  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }
  
  # check if user has provided the name of the data.frame to be subsetted
  if(is.null(df.name)){
    stop("Please provide the name of the data.frame to be filled as a character string: eg 'xxx'", call.=FALSE)
  }
  
  # if no value spcified for output object, then specify a default
  if(is.null(newobj)){
    newobj <- paste0(df.name,"_filled")
  }
  
  # check if the input object is defined in all the studies
  defined <- isDefined(datasources, df.name)
  
  # if the input object is not defined in any study then return an error message
  if(defined == FALSE){
    stop("The dataframe is not defined in all the studies!", call.=FALSE)
  }
  
  # call the internal function that checks the input object is of the same class in all studies.
  typ <- checkClass(datasources, df.name)
  
  # if the input object is not a matrix or a dataframe stop
  if(typ != 'data.frame' & typ != 'matrix'){
    stop("The input vector must be of type 'data.frame' or a 'matrix'!", call.=FALSE)
  }
  
  column.names <- list()
  for (i in 1:length(datasources)){
    column.names[[i]] <- ds.colnames(df.name, datasources=datasources[[i]])
  }
  
  allNames <- unique(unlist(column.names))
  
  if(!is.null(allNames)){
    allNames.transmit <- paste(allNames,collapse=",")
  }else{
    allNames.transmit <- NULL
  }
  
  calltext <- call("dataFrameFillDS", df.name, allNames.transmit)
  opal::datashield.assign(datasources, newobj, calltext)
  
  #############################################################################################################
  # DataSHIELD CLIENTSIDE MODULE: CHECK KEY DATA OBJECTS SUCCESSFULLY CREATED
  
  # SET APPROPRIATE PARAMETERS FOR THIS PARTICULAR FUNCTION
  test.obj.name <- newobj	
						
  # CALL SEVERSIDE FUNCTION
  calltext <- call("testObjExistsDS", test.obj.name)
  object.info <- opal::datashield.aggregate(datasources, calltext)	
  
  # CHECK IN EACH SOURCE WHETHER OBJECT NAME EXISTS	
  # AND WHETHER OBJECT PHYSICALLY EXISTS WITH A NON-NULL CLASS
  num.datasources <- length(object.info)

  obj.name.exists.in.all.sources <- TRUE
  obj.non.null.in.all.sources <- TRUE
  
  for(j in 1:num.datasources){			
    if(!object.info[[j]]$test.obj.exists){
      obj.name.exists.in.all.sources <- FALSE
    }
    if(object.info[[j]]$test.obj.class=="ABSENT"){
      obj.non.null.in.all.sources <- FALSE
    }
  }
  
  if(obj.name.exists.in.all.sources && obj.non.null.in.all.sources){
    return.message <-	paste0("A data object <", test.obj.name, "> has been created in all specified data sources")
  }else{
    return.message.1 <- paste0("Error: A valid data object <", test.obj.name, "> does NOT exist in ALL specified data sources")
    return.message.2 <- paste0("It is either ABSENT and/or has no valid content/class, see return.info above")
    return.message.3 <-	paste0("Please use ds.ls() to identify where missing")
    return.message <- list(return.message.1, return.message.2, return.message.3)
  }
  
  calltext <- call("messageDS", test.obj.name)
  studyside.message <- opal::datashield.aggregate(datasources, calltext)
  	
  no.errors <- TRUE
  for(nd in 1:num.datasources){
    if(studyside.message[[nd]]!="ALL OK: there are no studysideMessage(s) on this datasource"){
      no.errors <- FALSE
    }
  }
  
  if(no.errors){
    validity.check <- paste0("<",test.obj.name, "> appears valid in all sources")
    return(list(is.object.created=return.message, validity.check=validity.check))
  }
  
  if(!no.errors){
    validity.check <- paste0("<",test.obj.name,"> invalid in at least one source. See studyside.messages:")
    return(list(is.object.created=return.message, validity.check=validity.check, studyside.messages=studyside.message))
  }
  
  # END OF CHECK OBJECT CREATED CORRECTLY MODULE
  #############################################################################################################
  
}
# ds.dataFrameFill
