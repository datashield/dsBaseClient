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
#' @export
#' @examples {
#' 
#' # load that contains the login details
#' data(logindata)
#' 
#' # login and assign specific variable(s)
#' myvar <- list("LAB_HDL")
#' opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#' # Example 1: plot a combined histogram of the variable 'LAB_HDL' - default behaviour
#' ds.quantileMean(x='D$LAB_HDL')
#' 
#' # Example 2: Plot the histograms separately (one per study)
#' ds.quantileMean(x='D$LAB_HDL', type='split')
#' 
#' }
#'
ds.quantileMean <- function(x=NULL, type='combine', datasources=NULL, ){
  
  # if no opal login details were provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    findLogin <- getOpals()
    if(findLogin$flag == 1){
      datasources <- findLogin$opals
    }else{
      if(findLogin$flag == 0){
        stop(" Are yout logged in to any server? Please provide a valid opal login object! ", call.=FALSE)
      }else{
        message(paste0("More than one list of opal login object were found: '", paste(findLogin$opals,collapse="', '"), "'!"))
        userInput <- readline("Please enter the name of the login object you want to use: ")
        datasources <- eval(parse(text=userInput))
        if(class(datasources[[1]]) != 'opal'){
          stop("End of process: you failed to enter a valid login object", call.=FALSE)
        }
      }
    }
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
  quants <- datashield.aggregate(datasources, as.symbol(cally1))
  
  # combine the vector of quantiles - using weighted sum
  cally2 <- paste0('length('), x, ')') 
  lengths <- datashield.aggregate(datasources, as.symbol(cally2))
  global.quantiles <- rep(0, length(quants[[1]])-1)
  global.mean <- 0
  for(i in 1: length(datasources)){
    vect <- quants[[i]][1:7] * lengths[[i]]
    global.quantiles <- global.quantiles + vect
    global.mean <- global.mean + quants[[i]][8] * lengths[[i]]
  } 
  global.mean <- global.mean/sum(unlist(lengths))
  global.quantiles <- global.quantiles/sum(unlist(lengths))
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
