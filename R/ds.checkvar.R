#'
#' @title Checks if all variables do exist and are not empty
#' @description This function checks that the variables to analyse are (1) available from all 
#' the studies and (2) that they do not contain only missing values (NAs). It excludes studies 
#' that fail any of these two checks.
#' @details if this function is given a variable(s) created/derived from a variable(s) initially
#' assigned using through 'datashield.login' only the second check is carried out as it is not then 
#' necessary to check is the variable is available.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal 
#' datasources.
#' @param variables a character vector, the names of the variable(s) to check 
#' @return the opal objects which passed the checks
#' @author Gaye, A.
#' @export
#' @examples {
#' 
#' # load that contains the login details
#' data(logindata)
#' 
#' # login and assign specific variable(s)
#' myvar <- list("LAB_TSC", "LAB_HDL")
#' opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#' # Example 1: run checks for the variable LAB_TSC assigned above
#' ds.checkvar(datasources=opals, variables=list(quote(D$LAB_TSC)))
#' 
#' # Example 2: lets assign LAB_TSC to another variable 'tsc' not attached to 'D' and check 'tsc'
#' datashield.assign(opals, 'tsc', quote(D$LAB_TSC))
#' ds.checkvar(datasources=opals, variables=list(quote(tsc)))
#' 
#' # Example 3: assign an empty variable and run the checks (uncommment the last line)
#' server <- "study1"
#' url <- "http://54.242.140.255"
#' user <- "administrator"
#' password <- "password"
#' table <- "Test.CNSIM"
#' logindata <- data.frame(cbind(server,url,user,password,table))
#' myvar <- list("LAB_TSC")
#' opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' #ds.checkvar(datasources=opals, variables=list(quote(D$LAB_TSC)))
#' }
#'
ds.checkvar <- function(datasources=NULL, variables=NULL){
  
  if(is.null(datasources)){
    message("\n\n ALERT!\n")
    message(" No valid opal object(s) provided.\n")
    message(" Make sure you are logged in to valid opal server(s).\n")
    stop(" End of process!\n\n", call.=FALSE)
  }
  
  if(is.null(variables)){
    message("\n\n ALERT!\n")
    message(" Please provide a list for the argument 'variables'\n")
    stop(" End of process!\n\n", call.=FALSE)
  }
  
  # print a message for the user informing of checks
  message("\nChecks are carried out on the variables used for the analysis\nto ensure they are available from the dataset(s) and not empty.\n")
  
  # names of the studies
  stdnames <- names(datasources)
  
  # global checks results holder
  track <- c()
  
  # loop through the input variables and check each in all the datasources
  for(i in 1:length(variables)){
    
    # get the variable and check if it is given in the form 'D$name'
    var2check <- variables[[i]]
    inputterm <- unlist(strsplit(deparse(var2check), "\\$", perl=TRUE))
    
    # a vector that keeps the results of the checks for each study
    toremove <- c()
    
    if(length(inputterm) > 1){
      # name of the variable
      varname <- strsplit(deparse(var2check), "\\$", perl=TRUE)[[1]][2]
      
      # call the function that checks if the variable is present in the assigned dataset
      presence <- ds.isPresent(datasources, var2check)
      
      # call the function that checks if the variable is present in the assigned dataset
      emptyness <- ds.isNA(datasources, var2check)
      
      for(i in 1:length(datasources)){
        if( !(presence[[i]]) || (emptyness[[i]])){
          toremove <- append(toremove, 1)
        }else{
          toremove <- append(toremove, 0)
        }
      }
    }else{
      # name of the variable
      varname <- deparse(var2check)
      # if the variable is not given in the form 'D$name' no checks for presence required
      emptyness <- ds.isNA(datasources, var2check)
      for(i in 1:length(datasources)){
        if(emptyness[[i]]){
          toremove <- append(toremove, 1)
        }else{
          toremove <- append(toremove, 0)
        }
      }     
    }
    
    # the studies where the checked variable is missing or completly empty 
    # are removed from the studies to analyse.
    studies2rm <- which(toremove == 1)
    if(length(studies2rm) > 0){
      track <- append(track, studies2rm)
    }else{
      #message("checks went fine for ", varname)
    }
  }
  
  # check final results and display final message
  final.res <- unique(track)
  if(!(is.null(final.res))){
    #datasources <- datasources[-final.res] 
    if(length(datasources) == 0 ){
      #stop("\nAll the studies failed the checks for one or more variables!\nTHE ANALYSIS CANNOT BE CARRIED OUT!\n")
    }else{
      #warning(paste(stdnames[final.res]), " will not be included in the analysis!\n")
    }
  }else{
   # message("All the variables are available and none has 'NA' at all its entries!\n")
  }
  return(datasources)
}

