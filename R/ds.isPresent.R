#' 
#' @title Checks if a vector is in a table assigned to R
#' @details the check is carried out only if the symbol of the table to search in
#' is provided - this is done by proving the second argument in the form 'X$nameOfVariable'
#' where 'X' is the name of the table/dataframe assigned to R from opal.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal. 
#' @param var2check a numerical or character vector
#' @return a boolean 'TRUE' if the vector is present and 'FALSE'  otherwise.
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
#'  # check if all entries of  variable 'LAB_HDL' hold missing values (NA)
#'  ds.isPresent(datasources=opals, var2check=quote(D$LAB_HDL))
#' }
#' 
ds.isPresent <- function(datasources=NULL, var2check=NULL){
    
   if(is.null(datasources)){
     cat("\n\n ALERT!\n")
     cat(" No valid opal object(s) provided.\n")
     cat(" Make sure you are logged in to valid opal server(s).\n")
     stop(" End of process!\n\n", call.=FALSE)
   }
   
   if(is.null(var2check)){
     cat("\n\n ALERT!\n")
     cat(" Please provide a list for the argument 'var2check'\n")
     stop(" End of process!\n\n", call.=FALSE)
   }

   # get the names of the opal servers/studies
   stdname <- names(datasources)
   
   # get the names of the variables to check
   inputterm <- unlist(strsplit(deparse(var2check), "\\$", perl=TRUE))
   
   if(length(inputterm) > 1){
     # get the names of the variables to check
     varID <- strsplit(deparse(var2check), "\\$", perl=TRUE)[[1]][2]
     
     # loop through the dataset(s) and the variable(s)
     symbol <- strsplit(deparse(var2check), "\\$", perl=TRUE)[[1]][1]
     
     # keep of the results of the checks for each study
     track <- c()

     for(i in 1: length(datasources)){
     
     # get the names of the variables in the assigned dataset
       var.names <- datashield.aggregate(datasources[i],  paste0('colnames(',symbol,')'))   
       
       # check if the variable is present from the assigned dataset
       presence  <- varID %in% unlist(var.names)
       
       if(presence){
         # record that the study passed the check
         track <- append(track, TRUE)
       }else{
         # record that the study failed the check and print a message
         track <- append(track, FALSE)
         cat("The variable(s)", varID, "is/are missing from", stdname[i],"!\n") 
       }

     }
     return(track)
   }else{
     cat("No assigned dataset to look in for this variable!\n") 
     return(NULL)
   }
   
}
