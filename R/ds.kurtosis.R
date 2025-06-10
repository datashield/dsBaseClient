#' 
#' @title Calculates the kurtosis of a numeric variable
#' @description This function calculates the kurtosis of a numeric variable.
#' @details The function calculates the kurtosis of an input variable x with three different methods. 
#' The method is specified by the argument \code{method}. If x contains any missings, the function removes those before
#' the calculation of the kurtosis. If \code{method} is set to 1 the following formula is used
#' \eqn{ kurtosis= \frac{\sum_{i=1}^{N} (x_i - \bar(x))^4 /N}{(\sum_{i=1}^{N} ((x_i - \bar(x))^2) /N)^(2) } - 3},
#' where \eqn{ \bar{x} } is the mean of x and \eqn{N} is the number of observations. If \code{method} is set to 2
#' the following formula is used \eqn{ kurtosis= ((N+1)*(\frac{\sum_{i=1}^{N} (x_i - \bar(x))^4 /N}{(\sum_{i=1}^{N} ((x_i - \bar(x))^2) /N)^(2) } - 3) + 6)*((N-1)/((N-2)*(N-3)))}.
#' If \code{method} is set to 3 the following formula is used \eqn{ kurtosis= (\frac{\sum_{i=1}^{N} (x_i - \bar(x))^4 /N}{(\sum_{i=1}^{N} ((x_i - \bar(x))^2) /N)^(2) })*(1-1/N)^2 - 3}.
#' This function is similar to the function \code{kurtosis} in R package \code{e1071}.
#' @param x a string character, the name of a numeric variable.
#' @param method an integer between 1 and 3 selecting one of the algorithms for computing kurtosis
#' detailed below. The default value is set to 1.
#' @param type a character which represents the type of analysis to carry out. 
#' If \code{type} is set to 'combine', 'combined', 'combines' or 'c', the global kurtosis is returned 
#' if \code{type} is set to 'split', 'splits' or 's', the kurtosis is returned separately for each study.
#' if \code{type} is set to 'both' or 'b', both sets of outputs are produced.
#' The default value is set to 'both'.
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} objects obtained after login. 
#' If the \code{datasources} argument is not specified 
#' the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#' @return a matrix showing the kurtosis of the input numeric variable, the number of valid observations and
#' the validity message.
#' @author Demetris Avraam, for DataSHIELD Development Team
#' @export
#' 
ds.kurtosis <- function(x=NULL, method=1, type='both', datasources=NULL){
  
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
  if(type != 'combine' & type != 'split' & type != 'both'){
    stop('Function argument "type" has to be either "both", "combine" or "split"', call.=FALSE)
  }
  
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
    calltext.split <- call("kurtosisDS1", x, method)
    output.split <- DSI::datashield.aggregate(datasources, calltext.split) 
    mat.split <- matrix(as.numeric(matrix(unlist(output.split), nrow=length(datasources), byrow=TRUE)[,1:2]),nrow=length(datasources))
    validity <- matrix(unlist(output.split), nrow=length(datasources), byrow=TRUE)[,3]
    mat.split <- data.frame(cbind(mat.split, validity))
    rownames(mat.split) <- names(output.split)
    colnames(mat.split) <- c('Kurtosis', 'Nvalid', 'ValidityMessage')
  }
  
  if (type=='combine' | type=='both'){
    global.mean <- ds.mean(x, type='combine')$Global.Mean[,'EstimatedMean']
    #global.var <- ds.var(x, type='combine')$Global.Variance[,'EstimatedVar']
    
    if (is.na(global.mean)){
      stop("FAILED: The number of valid observations in one or more studies is less than nfilter.tab. \n Check that by using the argument type=='split'", call.=FALSE)
    }else{
      calltext.combined <- call("kurtosisDS2", x, global.mean)
      output.combined <- DSI::datashield.aggregate(datasources, calltext.combined) 
      
      Global.sum.quartics <- 0
      Global.sum.squares <- 0
      Global.Nvalid <- 0
      for (s in 1:length(datasources)){
        Global.sum.quartics <- Global.sum.quartics + output.combined[[s]]$Sum.quartics
        Global.sum.squares <- Global.sum.squares + output.combined[[s]]$Sum.squares
        Global.Nvalid <- Global.Nvalid + output.combined[[s]]$Nvalid
      }
      
      g2.global <- ((Global.sum.quartics/Global.Nvalid)/(Global.sum.squares/Global.Nvalid)^(2))-3
      
      if(method==1){
        Global.kurtosis <- g2.global
        combinedMessage <- "VALID ANALYSIS"
      }  
      if(method==2){
        Global.kurtosis <- ((Global.Nvalid + 1) * g2.global + 6) * (Global.Nvalid - 1)/((Global.Nvalid - 2) * (Global.Nvalid - 3))
        combinedMessage <- "VALID ANALYSIS"
      }  
      if(method==3){
        Global.kurtosis <- (g2.global + 3) * (1 - 1/Global.Nvalid)^2 - 3
        combinedMessage <- "VALID ANALYSIS"
      } 
      mat.combined <- data.frame(cbind(Global.kurtosis, Global.Nvalid, combinedMessage))
      rownames(mat.combined) <- 'studiesCombined'
      colnames(mat.combined) <- c('Kurtosis', 'Nvalid', 'ValidityMessage')
      
    }
  }
  
  if (type=='split'){
    return(Kurtosis.by.Study=mat.split)
  }
  
  if (type=="combine") {
    return(Global.Kurtosis=mat.combined)
  }
  
  if (type=="both") {
    return(list(Kurtosis.by.Study=mat.split, Global.Kurtosis=mat.combined))
  }
  
}
