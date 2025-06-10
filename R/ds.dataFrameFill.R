#'
#' @title Creates missing values columns in the server-side
#' @description Adds extra columns with missing values in a data frame on the server-side. 
#' @details This function checks if the input data frames have the same variables (i.e. the same
#' column names) in all of the used studies. When a study does not have some of the variables, the
#' function generates those variables as vectors of missing values and combines them as columns to
#' the input data frame. If any of the generated variables are of class factor, the function 
#' assigns to those the corresponding levels of the factors given from the studies where such 
#' factors exist.
#' 
#' Server function called: \code{dataFrameFillDS}
#' @param df.name a character string representing the name of the input data frame that will be
#' filled with extra columns of missing values. 
#' @param newobj a character string that provides the name for the output data frame  
#' that is stored on the data servers. Default value is "dataframefill.newobj". 
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} objects obtained after login. 
#' If the \code{datasources} argument is not specified 
#' the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#' @return \code{ds.dataFrameFill} returns the object specified by the \code{newobj} argument which 
#' is written to the server-side. Also, two validity messages are returned to the
#' client-side indicating the name of the \code{newobj} that has been created in each data source
#' and if it is in a valid form.
#' @author Demetris Avraam for DataSHIELD Development Team
#' 
#' @examples 
#' \dontrun{
#' 
#'   ## Version 6, for version 5 see the Wiki 
#'   # Connecting to the Opal servers
#' 
#'   require('DSI')
#'   require('DSOpal')
#'   require('dsBaseClient')
#' 
#'   builder <- DSI::newDSLoginBuilder()
#'   builder$append(server = "study1", 
#'                  url = "http://192.168.56.100:8080/", 
#'                  user = "administrator", password = "datashield_test&", 
#'                  table = "CNSIM.CNSIM1", driver = "OpalDriver")
#'   builder$append(server = "study2", 
#'                  url = "http://192.168.56.100:8080/", 
#'                  user = "administrator", password = "datashield_test&", 
#'                  table = "CNSIM.CNSIM2", driver = "OpalDriver")
#'   builder$append(server = "study3",
#'                  url = "http://192.168.56.100:8080/", 
#'                  user = "administrator", password = "datashield_test&", 
#'                  table = "CNSIM.CNSIM3", driver = "OpalDriver")
#'                  
#'   logindata <- builder$build()
#'   
#'   # Log onto the remote Opal training servers
#'   connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D") 
#'   
#'   # Create two data frames with one different column
#'   
#'   ds.dataFrame(x = c("D$LAB_TSC","D$LAB_TRIG","D$LAB_HDL",
#'                      "D$LAB_GLUC_ADJUSTED","D$PM_BMI_CONTINUOUS"),
#'                newobj = "df1",
#'                datasources = connections[1])
#'                
#'   ds.dataFrame(x = c("D$LAB_TSC","D$LAB_TRIG","D$LAB_HDL","D$LAB_GLUC_ADJUSTED"),
#'                newobj = "df1",
#'                datasources = connections[2])
#'   
#'   # Fill the data frame with NA columns
#'   
#'   ds.dataFrameFill(df.name = "df1",
#'                    newobj = "D.Fill",
#'                    datasources = connections[c(1,2)]) # Two servers are used
#'
#'
#'   # Clear the Datashield R sessions and logout
#'   datashield.logout(connections) 
#' }
#' @export
#'
ds.dataFrameFill <- function(df.name=NULL, newobj=NULL, datasources=NULL){

  # if no connections details are provided look for 'connection' objects in the environment
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }

  # check if user has provided the name of the data.frame to be subsetted
  if(is.null(df.name)){
    stop("Please provide the name of the data.frame to be filled as a character string: eg 'xxx'", call.=FALSE)
  }

  # if no value spcified for output object, then specify a default
  if(is.null(newobj)){
    newobj <- "dataframefill.newobj"
  }

  # check if the input dataframe is defined in all the studies
  defined <- isDefined(datasources, df.name)

  # call the internal function that checks the input object is of the same class in all studies.
  typ <- checkClass(datasources, df.name)

  # if the input object is not a matrix or a dataframe stop
  if(!('data.frame' %in% typ) && !('matrix' %in% typ)){
    stop("The input vector must be of type 'data.frame' or a 'matrix'!", call.=FALSE)
  }

  column.names <- lapply(datasources, function(dts){DSI::datashield.aggregate(dts, call("colnamesDS", df.name))})
  
  allNames <- unique(unlist(column.names))

  # if the datasets share the same variables then the function stops
  check.indicator <- c()
  for (i in 1:length(datasources)){
    if(length(setdiff(allNames,column.names[[i]])) > 0){
      check.indicator[i] <- 1
    }else{
      check.indicator[i] <- 0}
  }

  if(sum(check.indicator)==0){
    stop("The dataframes have the same variables. There are no missing variables to fill!", call.=FALSE)
  }

  if(!is.null(allNames)){
    allNames.transmit <- paste(allNames,collapse=",")
  }else{
    allNames.transmit <- NULL
  }
  
  defined.list <- lapply(allNames, function(x){isDefined(datasources=datasources, obj=paste0(df.name, '$', x), error.message=FALSE)})
  defined.vect1 <- lapply(defined.list, function(x){unlist(x)})
  defined.vect2 <- lapply(defined.vect1, function(x){which(x == FALSE)})
  
  # get the class of each variable in the dataframes
  class.list <- lapply(allNames, function(x){lapply(datasources, function(dts){DSI::datashield.aggregate(dts, call('classDS', paste0(df.name, '$', x)))})})
  class.vect1 <- lapply(class.list, function(x){unlist(x)})
  # the loop below is to avoid autocompletion of variable name
  for (i in 1:length(allNames.transmit)){
    if(length(defined.vect2[[i]])>0){class.vect1[[i]][defined.vect2[[i]]]<-'NULL'}
  }
  class.vect2 <- lapply(class.vect1, function(x){x[which(x != 'NULL')[[1]]]})
  class.vect2 <- unname(unlist(class.vect2))
  
  # check if any of the elements in class.vect2 are factor
  # and if yes then get their levels
  df.indicator <- list()
  levels.vec <- list()
  if(length(class.vect2)>0){
    anyFactors <- allNames[which(class.vect2 == 'factor')]
    if(length(anyFactors)>0){
      for(i in 1:length(anyFactors)){
        df.indicator[[i]] <- lapply(column.names, function(x){is.element(anyFactors[i],unlist(x))})
      }
      df.indicator.index <- lapply(df.indicator, function(x){which(x==TRUE)})
      for(i in 1:length(anyFactors)){
        levels.vec[[i]] <- dsBaseClient::ds.levels(x=paste0(df.name, '$', anyFactors[i]), datasources=datasources[df.indicator.index[[i]]])
        levels.vec[[i]] <- as.numeric(levels.vec[[i]][[1]][[1]])
      }
    }
  }
  levels.vec.transmit <- unlist(lapply(levels.vec, function(x){paste(x,collapse=",")}))
  
  if(!is.null(class.vect2)){
    class.vect.transmit <- paste(class.vect2,collapse=",")
  }else{
    class.vect.transmit <- NULL
  }
  
  calltext <- call("dataFrameFillDS", df.name, allNames.transmit, class.vect.transmit, levels.vec.transmit)
  DSI::datashield.assign(datasources, newobj, calltext)

  #############################################################################################################
  # DataSHIELD CLIENTSIDE MODULE: CHECK KEY DATA OBJECTS SUCCESSFULLY CREATED

  # SET APPROPRIATE PARAMETERS FOR THIS PARTICULAR FUNCTION
  test.obj.name <- newobj

  # CALL SEVERSIDE FUNCTION
  calltext <- call("testObjExistsDS", test.obj.name)
  object.info <- DSI::datashield.aggregate(datasources, calltext)

  # CHECK IN EACH SOURCE WHETHER OBJECT NAME EXISTS
  # AND WHETHER OBJECT PHYSICALLY EXISTS WITH A NON-NULL CLASS
  num.datasources <- length(object.info)

  obj.name.exists.in.all.sources <- TRUE
  obj.non.null.in.all.sources <- TRUE

  for(j in 1:num.datasources){
    if(!object.info[[j]]$test.obj.exists){
      obj.name.exists.in.all.sources <- FALSE
    }
    if(is.null(object.info[[j]]$test.obj.class) || ("ABSENT" %in% object.info[[j]]$test.obj.class)){
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
  studyside.message <- DSI::datashield.aggregate(datasources, calltext)

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
