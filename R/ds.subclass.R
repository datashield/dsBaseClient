#' 
#' @title Generates valid subset(s) of a dataframe or a factor
#' @description The function takes a categorical variable or a data frame as input and generates subset(s)
#' variables or data frames for each category.
#' @details If the input data object is a data frame it is possible to specify  the variables  
#' to subset on. If a subset is not 'valid' all its the values are reported as missing (i.e. NA),
#' the name of the subsets is labelled as '_INVALID'. Subsets are considered invalid if the number
#' of observations it holds are less than the agreed threshold (e.g. 5). Subsets with no observations
#' (i.e. no observation in that categorie) are assigned missing values. The user is informed if any 
#' subset is invalid or empty.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @param subsets the name of the output object, a list that holds the subset objects. If set to NULL
#' the default name of this list is 'subsclasses' 
#' @param data a string character, the name of the data frame or the vector to generate subsets from.
#' @param variables a vector of string characters, the name(s) of the variables to subset by.
#' @return a no data are return to the user but messages are printed out.
#' @author Gaye, A.
#' @export
#' @examples {
#'
#' # load the login data
#' library(opal)
#' data(logindata)
#' 
#' # login and assign some variables to R
#' myvar <- list("DIS_DIAB","PM_BMI_CONTINUOUS","LAB_HDL", "GENDER")
#' opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#' # Example 1: generate all possible subsets from the table assigne above 
#' ds.subclass(datasources=opals, subsets="subclasses", data="D")
#' 
#' # Example 2: subset the table initially assigned by the variable 'GENDER'
#' ds.subclass(datasources=opals, subsets="subtables", data="D", variables="GENDER")
#' 
#' # Example 3: generate a new variable 'gender' and split it into two vectors: males and females
#' datashield.assign(opals, "gender", quote(D$GENDER))
#' ds.subclass(datasources=opals, subsets="mf.tables", data="gender")
#' 
#' ds.subclass(datasources=opals, subsets="subvectors", data=NULL, variables=NULL)
#' }
#' 
ds.subclass <- function(datasources=NULL, subsets="subsclasses", data=NULL, variables=NULL){
  
  if(is.null(datasources)){
    message("No valid opal object(s) provided!")
    message("Make sure you are logged in to valid opal server(s).")
    stop(" End of process!\n", call.=FALSE)
  }else{
    stdnames <- names(datasources)
  }
  
  # call the server side function that does the job
  # get the indices of the columns refered to by their names in the arguments
  cally <- call('subclassDS', data, variables)
  datashield.assign(datasources, subsets, cally)
  
  # possible 'error' from the server side function if no subset has been generated
  m1 <- "The input data you provided is not defined!"
  m2 <- "The input table holds no factor variables!"
  m3 <- "The variables to subset by must be factors!"
  m4 <- "The input data must be a factor or a dataframe!"
  txt2print <- c(m1, m2, m3, m4)
  
  # a message so the user knows the function was ran (assign functions are 'silent')
  message("An 'assign' function was ran, no output should be expected on the client side!")
  
  
  # check the subsets and tell if they have been generated or if some are invalid or empty
  for(i in 1: length(datasources)){
    listcontent <- ds.names(datasources[i], subsets)
    if(listcontent[[1]] == m1 | listcontent[[1]] == m2 | listcontent[[1]] == m3 | listcontent[[1]] == m4){
      for(q in 1:4){
        if(listcontent[[1]] == txt2print[q]){
          message(txt2print[q])
        }
      }
    }else{
      invalidsubs <- c()
      emptysubs <- c()
      for(j in 1:length(listcontent)){
        check1 <- which(unlist(strsplit(listcontent[[j]],"_")) == "INVALID")
        check2 <- which(unlist(strsplit(listcontent[[j]],"_")) == "NULL")
        if(length(check1) > 0){
          invalidsubs <- append(invalidsubs, listcontent[[j]])
        }
        if(length(check2) > 0){
          emptysubs <- append(emptysubs, listcontent[[j]])
        }
      }
      if(length(invalidsubs) > 0){
        message(paste0("Invalids subsets in ", stdnames[i], ":"))
        print(invalidsubs)
      }
      if(length(emptysubs) > 0){
        message(paste0("Empty subsets (i.e. no observations)  in ", stdnames[i], ":"))
        print(emptysubs)
      }
    }
  }
}
