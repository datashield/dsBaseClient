#'
#' @title  Computes the mean and standard deviation across categories
#' @description This function calculates the mean and  the standard deviation (SD)
#'  of a continuous variable for each class of up to 3 categorical variables.
#' @details The function splits the input dataset into subsets (one for each category) and calculates
#' the mean and SD of the specified numeric variables. It is important to note that the process of
#' generating the final table(s) can be time consuming particularly if the subsetting is done across
#' more than one categorical variable and the run-time lengthens if the parameter \code{type} is set to
#' \code{'split'} as a table is then produced for each study. It is therefore advisable to run the function
#' only for the studies of the user interested in but including only those studies in the
#' parameter \code{datasources}.
#' 
#'  Depending on the variable \code{type} can be carried out two analysis:\cr
#'  (1) \code{'combine'}: a pooled table of results is generated. \cr
#'  (2) \code{'split'}: a table of results is generated for each study. 
#'  
#' 
#' @param x a character string specifying the name of the dataset or a text formula.
#' @param outvar a character vector specifying the names of the continuous variables.
#' @param covar a character vector specifying the names of up to 3 categorical variables
#' @param type a character string that represents the type of analysis to carry out.
#' \code{type} can be set as: \code{'combine'} or \code{'split'}. 
#' Default \code{'combine'}. 
#' For more information see \strong{Details}. 
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} 
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#' @return \code{ds.meanByClass} returns to the client-side a table or a list of tables that 
#' hold the length of the numeric variable(s) and their mean
#' and standard deviation in each subgroup (subset).
#' @export
#' @author DataSHIELD Development Team
#' @seealso \code{\link{ds.subsetByClass}} to subset by the classes of factor vector(s).
#' @seealso \code{\link{ds.subset}} to subset by complete cases (i.e. removing missing values), threshold, columns and rows.
#' @examples
#' \dontrun{
#'
#'  ## Version 6, for version 5 see the Wiki
#'   
#'   # connecting to the Opal servers
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
#'   logindata <- builder$build()
#'   
#'   
#'   connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D") 
#'   
#'   #Calculate mean by class
#'   
#'   ds.meanByClass(x = "D",
#'                  outvar = c('LAB_HDL','LAB_TSC'),
#'                  covar = c('PM_BMI_CATEGORICAL'),
#'                  type = "combine",
#'                  datasources = connections)
#'                  
#'   ds.meanByClass(x = "D$LAB_HDL~D$PM_BMI_CATEGORICAL",
#'                  type = "combine",
#'                  datasources = connections[1])#Only the frist server is used ("study1")  
#'              
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(connections)
#' }
#'
ds.meanByClass <-  function(x=NULL, outvar=NULL, covar=NULL, type='combine', datasources=NULL){
  .Deprecated("ds.meanSdGp")

  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
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
