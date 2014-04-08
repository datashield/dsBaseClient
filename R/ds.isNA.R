#' 
#' @title Checks if a vector is empty 
#' @description this function is similar to R function \code{is.na} but instead of a vector 
#' of booleans it returns just one boolean to tell if all the elements are missing values.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal 
#' @param var2check a numerical or character vector
#' @return a boolean 'TRUE' if the vector contains on NAs and 'FALSE'  otherwise.
#' @author Gaye, A.
#' @export
#' @examples {
#' # load the login data
#' data(logindata)
#' 
#' # login and assign specific variable(s)
#' library(opal)
#' myvar <- list("LAB_HDL")
#' opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#'
#' # check if all the observation of the variable 'LAB_HDL' are missing (NA)
#' ds.isNA(datasources=opals, var2check=quote(D$LAB_HDL))
#' }
#' 
ds.isNA <- function(datasources=NULL, var2check=NULL){

   if(is.null(datasources)){
     message(" ALERT!")
     message(" No valid opal object(s) provided.")
     message(" Make sure you are logged in to valid opal server(s).")
     stop(" End of process!", call.=FALSE)
   }
   
   if(is.null(var2check)){
     message(" ALERT!")
     message(" Please provide a list for the argument 'var2check'")
     stop(" End of process!", call.=FALSE)
   }

   # get the names of the opal servers/studies
   stdname <- names(datasources)
   
   # get the names of the variables to check
   inputterm <- unlist(strsplit(deparse(var2check), "\\$", perl=TRUE))
   if(length(inputterm) > 1){
      varID <- strsplit(deparse(var2check), "\\$", perl=TRUE)[[1]][2]
   }else{
      varID <- deparse(var2check)
   }

   # keep of the results of the checks for each study
   track <- c()
   
   # call server side function 'isNA.ds' to check, in each study, if the vector is empty
   for(i in 1: length(datasources)){
     cally <- call("isNA.ds", var2check)
     out <- datashield.aggregate(datasources[i], cally)
     if(out[[1]]){ 
       track <- append(track, TRUE)
       message("The variable ", varID, " in ", stdname[i], " is empty (NAs only).")
     }else{
       track <- append(track, FALSE)
     }
   }
   return(track)
}
