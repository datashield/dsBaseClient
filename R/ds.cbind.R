#' 
#' @title Combines objects by columns
#' @description This is similar to the R base function \code{cbind} with the only differences 
#' that  it allows to combine up to 5 objects (vectors and/or table structures).
#' @details see details of the R base function \code{cbind}.
#' @param x a character vector, the name of the vector and or table to combine by column.
#' @param newobj the name of the output object. If this argument is set to \code{NULL}, 
#' the name of the new object is 'newCbindObject'.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @return nothing is returned to the client, the new object is stored on the server side.
#' @author Gaye, A.
#' @seealso \link{ds.dataframe} to generate a table of type dataframe.
#' @seealso \link{ds.changeRefGroup} to change the reference level of a factor.
#' @seealso \link{ds.colnames} to obtain the column names of a matrix or a data frame
#' @seealso \link{ds.asMatrix} to coerce an object into a matrix type.
#' @seealso \link{ds.dim} to obtain the dimensions of matrix or a data frame.
#' @export
#' @examples {
#' 
#'   # load the file that contains the login details
#'   data(logindata)
#'   library(opal)
#'
#'   # login and assign specific variables(s)
#'   # (by default the assigned dataset is a dataframe named 'D')
#'   myvar <- list('LAB_TSC', 'LAB_HDL')
#'   opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#'   # generate a new dataframe by combining the log values of 
#'   # 'LAB_TSC' and 'LAB_HDL', by columns
#'   ds.assign(toAssign='log(D$LAB_TSC)', newobj='labtsc')
#'   ds.assign(toAssign='log(D$LAB_HDL)', newobj='labhdl')
#'   ds.cbind(x=c('labtsc','labhdl'), newobj="myDataframe")
#' 
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(opals)
#' 
#' }
#' 
ds.cbind <- function(x=NULL, newobj='newCbindObject', datasources=NULL){

  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }
  
  # if not more than one input objects stop 
  if(length(x) < 2){
    stop("You must provide the names of at least two objects!", call.=FALSE)
  }  
  
  # check if the input object(s) is(are) defined in all the studies
  for(i in 1:length(x)){
    if(!(is.null(x[i]))){
      inputElts <- extract(x[i])
      if(is.na(inputElts[[1]])){
        defined <- isDefined(datasources, inputElts[[2]])
      }else{
        defined <- isDefined(datasources, inputElts[[1]])
        cally <- paste0("colnames(", inputElts[[1]], ")")
        column_names <- unique(unlist(opal::datashield.aggregate(datasources, cally)))
        if(!(inputElts[[2]] %in% column_names)){
          stop(paste0("No variable ",inputElts[[2]]," in ", inputElts[[1]], " in one or more studies"), call.=FALSE)
        }
      } 
    }
  }
  
  # call the server side function
  cally <-  paste0("cbind(", paste(x,collapse=","), ")")
  opal::datashield.assign(datasources, newobj, as.symbol(cally))
  
  # check that the new object has been created and display a message accordingly
  finalcheck <- isAssigned(datasources, newobj)
  
}