#' 
#' @title Retrieves the dimension of an object
#' @description this function is similar to R function \code{dim}
#' @details the function returns the unpooled or pooled dimension of the object by summing
#' up the individual dimensions returned from each study or the dimension of the object in each 
#' study. Unlike the other DataSHIELD function the default behaviour is to output the dimension 
#' of each study separately. 
#' @param x a character, the name of R table object, for example a matrix, array or data frame
#' @param type a character which represents the type of analysis to carry out. 
#' If \code{type} is set to 'combine', a global variance is calculated .
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @return for an array, \code{NULL} or a vector of mode \code{integer}
#' @author Gaye, A.; Isaeva, J.
#' @export
#' @examples {
#' 
#' # load that contains the login details
#' data(logindata)
#' 
#' # login and assign all the stored variables.
#' opals <- datashield.login(logins=logindata,assign=TRUE)
#' 
#' # Example 1: Get the dimension of the assigned datasets in each study
#' ds.dim(x='D')
#' 
#' # Example 2: Get the pooled dimension of the assigned datasets
#' ds.dim(x='D', type='combine')
#' 
#' # Example 2: Input has to be either matrix, data frame or an array
#' # In the below example, the inpout is a vector so it will not work.
#' \dontrun{ ds.dim(x='D$LAB_TSC') }
#' 
#' # clear the Datashield R sessions and logout
#' datashield.logout(opals)
#' 
#' }
#' 
ds.dim = function(x=NULL, type='split', datasources=NULL) {
  
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
    stop("Please provide a the name of a data.frame or matrix!", call.=FALSE)
  }
  
  # check if the input object(s) is(are) defined in all the studies
  defined <- isDefined(datasources, x)
  
  # call the internal function that checks the input object is of the same class in all studies.
  typ <- checkClass(datasources, x)
  
  # throw a message and stop if input is not table structure
  if(typ != 'data.frame' & typ!= 'matrix'){
    stop("The input object must be a table structure!", call.=FALSE)
  }
  
  cally <- paste0("dim(", x, ")")
  dimensions <- datashield.aggregate(datasources, as.symbol(cally))
  
  if(type=="combine"){
    global.dim1 <- 0
    global.dim2 <- dimensions[[1]][2]
    for(i in 1:length(datasources)){
      global.dim1 <- global.dim1 + dimensions[[i]][1]
    }
    return(list("pooled.dimension"=c(global.dim1, global.dim2)))
  }else{
    if(type=="split"){
      return(dimensions)
    }else{
      stop('Function argument "type" has to be either "combine" or "split"')
    } 
  }
}