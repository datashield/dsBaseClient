#'
#' @title searches for opal login object in the environment
#' @description This is an internal function required by a client function
#' @details if the user does not set the argument 'datasources', this function
#' is called to searches for opal login objects in the environment.
#' If only one opal object is found, it automatically becomes the default selection.
#' If more than one is found but one is called 'default.opals' then that is selected.
#' If more than one is found with none is called 'default.opals' the user is 
#' told that they can either specify a particular Opal using the 'datasources=' argument
#' that exists in every relevant datashield client-side function or else they
#' can use the 'ds.setDefaultOpals()' function to create a copy of a selected Opal objects
#' which is called 'default.opals' and is then selected by default in future calls to findLoginObjects.
#' If the default Opal object needs to be changed then 'ds.setDefaultOpals()' can be run again.
#' A previous version of 'findLoginObjects()' asked the user to specify which Opal to choose
#' if no default could be identified, but that did not work in all versions of R and so was removed.
#' @keywords internal
#' @return returns a list of opal login objects or stops the process
#' @author Amadou Gaye, Paul Burton (updated 15/10/18). THIS IS VERSION TO USE 8/2/19.
#' 
findLoginObjects <- function(){

  findLogin <- getOpals()

  if (findLogin$flag == 0){
    stop(" Are you logged in to any server? Please provide a valid opal login object! ", call.=FALSE)
  } 

  if(findLogin$flag == 1){
    datasources <- findLogin$opals
    return(datasources)
  }

  if(findLogin$flag > 1) {
    for(j in 1:findLogin$flag){
      if(findLogin$opals[[j]]=="default.opals"){
        datasources<-eval(parse(text=findLogin$opals[[j]]),envir=0)
        return(datasources)
	  }
	}
    message(paste0(" More than one list of opal login objects was found with no default specified:\n '", paste(findLogin$opals.list,collapse="', '"), "'!!"))
    stop(" \n\n Please specify a default Opal object using the following call syntax:\n ds.setDefaultOpals(opal.name='name of opal in inverted commas')\n\n",call.=FALSE) 
  }
	
}
#findLoginObjects
