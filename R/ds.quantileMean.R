#' 
#' @title Compute the quantiles 
#' @description This function calculate the mean and quantile values of a quantitative variable
#' @details Unlike standard r summary function the minimum and maximum values afe not returned
#' because they are potentially disclosive.
#' @param x a character, the name of the numeric vector.
#' @param type a character which represent the type of graph to display. 
#' If \code{type} is set to 'combine' pooled values are displayed and sumamries a
#' returned for each study if \code{type} is set to 'split'
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources. 
#' @return quantiles and statistical mean
#' @author Gaye, A.
#' @seealso \code{ds.mean} to compute statistical mean.
#' @seealso \code{ds.summary} to generate the summary of a variable.
#' @export
#' @examples
#' \dontrun{
#' 
#'   # load that contains the login details
#'   data(logindata)
#' 
#'   # login and assign specific variable(s)
#'   myvar <- list('LAB_HDL')
#'   opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#'   # Example 1: plot a combined histogram of the variable 'LAB_HDL' - default behaviour
#'   ds.quantileMean(x='D$LAB_HDL')
#' 
#'   # Example 2: Plot the histograms separately (one per study)
#'   ds.quantileMean(x='D$LAB_HDL', type='split')
#' 
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(opals)
#' 
#' }
#'
ds.quantileMean <- function(x=NULL, type='combine', datasources=NULL){
  
  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }
  
  if(is.null(x)){
    stop("Please provide the name of the input vector!", call.=FALSE)
  }
  
  # the input variable might be given as column table (i.e. D$x)
  # or just as a vector not attached to a table (i.e. x)
  # we have to make sure the function deals with each case
  xnames <- extract(x)
  varname <- xnames$elements
  obj2lookfor <- xnames$holders
  
  # check if the input object(s) is(are) defined in all the studies
  if(is.na(obj2lookfor)){
    defined <- isDefined(datasources, varname)
  }else{
    defined <- isDefined(datasources, obj2lookfor)
  }
  
  # call the internal function that checks the input object is of the same class in all studies.
  typ <- checkClass(datasources, x)
  
  # the input object must be a numeric or an integer vector
  if(typ != 'integer' & typ != 'numeric'){
    message(paste0(x, " is of type ", typ, "!"))
    stop("The input object must be an integer or numeric vector.", call.=FALSE)
  }
  
  # get the server function that produces the quantiles
  cally1 <- paste0('quantileMeanDS(', x, ')') 
  quants <- opal::datashield.aggregate(datasources, as.symbol(cally1))
  
  # combine the vector of quantiles - using weighted sum
  cally2 <- paste0('length(', x, ')') 
  lengths <- opal::datashield.aggregate(datasources, as.symbol(cally2)) 
  cally3 <- paste0("numNaDS(", x, ")")
  numNAs <- opal::datashield.aggregate(datasources, cally3)  
  global.quantiles <- rep(0, length(quants[[1]])-1)
  global.mean <- 0
  for(i in 1: length(datasources)){
    vect <- quants[[i]][1:7] * (lengths[[i]]-numNAs[[i]])
    global.quantiles <- global.quantiles + vect
    global.mean <- global.mean + quants[[i]][8] * (lengths[[i]]-numNAs[[i]])
  } 
  
  global.mean <- global.mean/(sum(unlist(lengths))-sum(unlist(numNAs)))
  global.quantiles <- global.quantiles/(sum(unlist(lengths))-sum(unlist(numNAs)))
  output <- c(global.quantiles, global.mean)
  names(output) <- c("5%","10%","25%","50%","75%","90%","95%","Mean")
                                             
  if(type=="combine"){
    message(" Quantiles of the pooled data")
    return(output)
  }else{
    if(type=="split"){
      return(quants)
    }else{
      stop('Function argument "type" has to be either "combine" or "split"')
    }
  }                                        
}
