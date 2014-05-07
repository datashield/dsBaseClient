#' 
#' @title Generates the summary of a numeric or factor vector
#' @details This function call \code{ds.quantilemean} if the input vector is a numeric
#' (i.e. continuous variable) and \code{ds.table1d} if it is a factor (i.e. categorical variable).
#' The user can choose the generate a pooled or split summary (one for each study).
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources. 
#' @param xvect a numeric or factor variable
#' @param type a character which represents the type of graph to display. 
#' If \code{type} is set to 'combine', a summary of the pooled data is displayed and 
#' if \code{type} is set to 'split', the summary is generate for each study separately.
#' @return a non disclosive summary
#' @author Gaye, A.
#' @export
#' @examples {
#' # load the login data
#' data(logindata)
#' 
#' # login and assign specific variable(s)
#' myvar <- list("LAB_TSC", "GENDER")
#' opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#' # Example 1: Produce the global summary of a continuous variable
#' ds.summary(datasources=opals, xvect=quote(D$LAB_TSC))
#' 
#' # Example 2: Produce the global summary of a factor variable
#' ds.summary(datasources=opals, xvect=quote(D$GENDER))
#' }
#' 
ds.summary <- function(datasources=NULL, xvect=NULL, type="combine"){
  
  if(is.null(datasources)){
    message(" ALERT!")
    message(" No valid opal object(s) provided.")
    message(" Make sure you are logged in to valid opal server(s).")
    stop(" End of process!", call.=FALSE)
  }
  
  if(is.null(xvect)){
    message(" ALERT!")
    message(" Please provide a valid numeric of character vector")
    stop(" End of process!", call.=FALSE)
  }
  
  # the input variable might be given as column table (i.e. D$xvect)
  # or just as a vector not attached to a table (i.e. xvect)
  # we have to make sure the function deals with each case
  inputterms <- unlist(strsplit(deparse(xvect), "\\$", perl=TRUE))
  if(length(inputterms) > 1){
    var <-  strsplit(deparse(xvect), "\\$", perl=TRUE)[[1]][2]
  }else{
    var <- deparse(xvect)
  }
  
  # get the names and number of the studies/datasources
  stdnames <- names(datasources)
  numstudies <- length(stdnames)
  
  # check the type of the variable in each study, if type differs accross studies stop
  cally <- call("class", xvect)
  class1 <-  datashield.aggregate(datasources[1], cally)
  class.of.var <- c()
  for(i in 1:numstudies){
    cally <- call("class", xvect)
    class.of.var <- datashield.aggregate(datasources[i], cally)
    if(class.of.var != class1[[1]]){
      stop("The variable is not of the same type across all the studies!")
    }
  }
                                  
  # now get the summary depending on the type of the input variable
  if(class1[[1]] == "numeric" | class1[[1]] == "integer"){
    if(type == "combine"){
      ds.quantilemean(datasources, xvect)
    }else{
      if(type=="split"){
        ds.quantilemean(datasources, xvect, type="split")
      }else{
        stop('Function argument "type" has to be either "combine" or "split"')
      }
    }
  }else{
    if(class1[[1]] == "factor"){
      if(type=="combine"){
        ds.table1d(datasources, xvect)
      }else{
        if(type=="split"){
          ds.table1d(datasources, xvect, type="split")
        }else{
          stop('Function argument "type" has to be either "combine" or "split"')
        }
      }
     }else{
      stop("The variable is not a numeric or a factor, the summary cannot be produced!")
    }
  }
}