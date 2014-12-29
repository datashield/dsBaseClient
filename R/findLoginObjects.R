#'
#' @title searches for opal login object in the environment
#' @description This is an internal function required by a client function
#' @details if the user does not set the argument 'datasources', this function
#' is called to searches for opal login objects in the environment. If more than one 
#' login object is found a prompt asks the user to choose one and if none is found
#' the process stops.
#' @keywords internal
#' @return returns a list of opal login objects or stops the process
#' @author Gaye, A.
#' 
findLoginObjects <- function(){

  findLogin <- getOpals()
  if(findLogin$flag == 1){
    datasources <- findLogin$opals
    return (datasources)
  }else{
    if(findLogin$flag == 0){
      stop(" Are you logged in to any server? Please provide a valid opal login object! ", call.=FALSE)
    }else{
      message(paste0("More than one list of opal login object were found: '", paste(findLogin$opals,collapse="', '"), "'!"))
      userInput <- readline("Please enter the name of the login object you want to use: ")
      datasources <- eval(parse(text=userInput))
      if(class(datasources[[1]]) == 'opal'){
        return (datasources)
      }else{
        stop("End of process: you failed to enter a valid login object", call.=FALSE)
      }
    }
  }

}