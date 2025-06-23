#' 
#' @title Calculates the skewness of a server-side numeric variable 
#' @description This function calculates the skewness of a numeric variable 
#' that is stored on the server-side (Opal server). 
#' @details This function is similar to the function \code{skewness} in R package \code{e1071}.
#' 
#' The function calculates the skewness of an input variable \code{x} 
#' with three different methods: \cr
#' (1)  If \code{method} is set to 1 the following formula is used \eqn{ skewness= \frac{\sum_{i=1}^{N} (x_i - \bar(x))^3 /N}{(\sum_{i=1}^{N} ((x_i - \bar(x))^2) /N)^(3/2) }},
#' where \eqn{ \bar{x} } is the mean of x and \eqn{N} is the number of observations.\cr
#' (2) If \code{method} is set to 2
#' the following formula is used \eqn{ skewness= \frac{\sum_{i=1}^{N} (x_i - \bar(x))^3 /N}{(\sum_{i=1}^{N} ((x_i - \bar(x))^2) /N)^(3/2) } * \frac{\sqrt(N(N-1)}{n-2}}.\cr
#' (3) If \code{method} is set to 3 the following formula is used \eqn{ skewness= \frac{\sum_{i=1}^{N} (x_i - \bar(x))^3 /N}{(\sum_{i=1}^{N} ((x_i - \bar(x))^2) /N)^(3/2) } * (\frac{N-1}{N})^(3/2)}.
#' 
#' The \code{type} argument can be set as follows:\cr
#' (1) If \code{type} is set to \code{'combine'}, \code{'combined'}, \code{'combines'} or \code{'c'}, 
#' the global skewness is returned.\cr
#' (2) If \code{type} is set to \code{'split'}, \code{'splits'} or \code{'s'}, 
#' the skewness is returned separately for each study.\cr
#' (3) If \code{type} is set to \code{'both'} or \code{'b'}, both sets of outputs are produced.\cr
#' 
#' If \code{x} contains any missing value, the function removes those before
#' the calculation of the skewness. 
#' 
#' Server functions called: \code{skewnessDS1} and \code{skewnessDS2}
#' 
#' @param x a character string specifying the name of a numeric variable.
#' @param method an integer value between 1 and 3 selecting one of the algorithms for computing skewness. 
#' For more information see \strong{Details}. The default value is set to 1.
#' @param type a character string which represents the type of analysis to carry out. 
#' \code{type} can be set as: \code{'combine'}, \code{'split'} or \code{'both'}. For more information
#' see \strong{Details}. 
#' The default value is set to \code{'both'}.
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} 
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#' @return \code{ds.skewness} returns a matrix showing the skewness of the input numeric variable,
#' the number of valid observations and the validity message.
#' @author Demetris Avraam, for DataSHIELD Development Team
#' @examples 
#' \dontrun{
#'   ## Version 6, for version 5 see the Wiki
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
#'   connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D") 
#'   
#'   #Calculate the skewness of LAB_TSC numeric variable for each study separately and combined
#'   
#'   ds.skewness(x = "D$LAB_TSC",
#'               method = 1, 
#'               type = "both",
#'              datasources = connections)
#'   
#'   # Clear the Datashield R sessions and logout                 
#'   DSI::datashield.logout(connections) 
#'   
#' } 
#' @export
#' 
ds.skewness <- function(x=NULL, method=1, type='both', datasources=NULL){
  
  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }

  if(is.null(x)){
    stop("Please provide the name of the input vector!", call.=FALSE)
  }
  
  if(!all(method %in% c(1,2,3))){
    stop("method must be an integer between 1 and 3", call.=FALSE)
  }

  # enable valid aliases for "type" argument                  
  if(type == 'combine' | type == 'combined' | type == 'combines' | type == 'c') type <- 'combine'
  if(type == 'split' | type == 'splits' | type == 's') type <- 'split'
  if(type == 'both' | type == 'b' ) type <- 'both'
  if(type != 'combine' & type != 'split' & type != 'both')
    stop('Function argument "type" has to be either "both", "combine" or "split"', call.=FALSE)

  # check if the input object is defined in all the studies
  isDefined(datasources, x)
  
  # call the internal function that checks the input object is of the same class in all studies.
  typ <- checkClass(datasources, x)
  
  # the input object must be a numeric or an integer vector
  if(typ != 'integer' & typ != 'numeric'){
    message(paste0(x, " is of type ", typ, "!"))
    stop("The input object must be an integer or numeric vector.", call.=FALSE)
  }
  
  if (type=='split' | type=='both'){
    calltext.split <- call("skewnessDS1", x, method)
    output.split <- DSI::datashield.aggregate(datasources, calltext.split) 
    mat.split <- matrix(as.numeric(matrix(unlist(output.split), nrow=length(datasources), byrow=TRUE)[,1:2]),nrow=length(datasources))
    validity <- matrix(unlist(output.split), nrow=length(datasources), byrow=TRUE)[,3]
    mat.split <- data.frame(cbind(mat.split, validity))
    rownames(mat.split) <- names(output.split)
    colnames(mat.split) <- c('Skewness', 'Nvalid', 'ValidityMessage')
  }

  if (type=='combine' | type=='both'){
    global.mean <- ds.mean(x, type='combine')$Global.Mean[,'EstimatedMean']
    #global.var <- ds.var(x, type='combine')$Global.Variance[,'EstimatedVar']
    
    if (is.na(global.mean)){
      stop("FAILED: The number of valid observations in one or more studies is less than nfilter.tab. \n Check that by using the argument type=='split'", call.=FALSE)
    }else{
      calltext.combined <- call("skewnessDS2", x, global.mean)
      output.combined <- DSI::datashield.aggregate(datasources, calltext.combined) 
      
      Global.sum.cubes <- 0
      Global.sum.squares <- 0
      Global.Nvalid <- 0
      for (s in 1:length(datasources)){
        Global.sum.cubes <- Global.sum.cubes + output.combined[[s]]$Sum.cubes
        Global.sum.squares <- Global.sum.squares + output.combined[[s]]$Sum.squares
        Global.Nvalid <- Global.Nvalid + output.combined[[s]]$Nvalid
      }
      
      g1.global <- (Global.sum.cubes/Global.Nvalid)/(Global.sum.squares/Global.Nvalid)^(3/2)
      
      if(method==1){
        Global.skewness <- g1.global
        combinedMessage <- "VALID ANALYSIS"
      }  
      if(method==2){
        Global.skewness <- g1.global * sqrt(Global.Nvalid*(Global.Nvalid-1))/(Global.Nvalid-2)
        combinedMessage <- "VALID ANALYSIS"
      }  
      if(method==3){
        Global.skewness <- g1.global * ((Global.Nvalid-1)/(Global.Nvalid))^(3/2)
        combinedMessage <- "VALID ANALYSIS"
      } 
      mat.combined <- data.frame(cbind(Global.skewness, Global.Nvalid, combinedMessage))
      rownames(mat.combined) <- 'studiesCombined'
      colnames(mat.combined) <- c('Skewness', 'Nvalid', 'ValidityMessage')
      
    }
  }
  
  if (type=='split'){
    return(Skewness.by.Study=mat.split)
  }
  
  if (type=="combine") {
    return(Global.Skewness=mat.combined)
  }
  
  if (type=="both") {
    return(list(Skewness.by.Study=mat.split, Global.Skewness=mat.combined))
  }
  
}
