#' 
#' @title Gets the opal objects
#' @description This is an internal function.
#' @details The function searches for a list containing object of type 'opal'
#' in the global environment; if more than one list is found it return the lastest.
#' This way no matter what the user calls his opal login object it will be captured. 
#' @keywords internal
#' @return a list of opal object obtained after login into the servers
#' @author Amadou Gaye, Paul Burton (updated 15/10/18). THIS IS VERSION TO USE 8/2/19.
getOpals <- function(){
  
  # get the names of all the objects in the current work environment
  objs <- ls(name=.GlobalEnv)
  
  # check which of the object is a list (the opal objects are kept in a list)
  if(length(objs) > 0){
    opalist <- vector('list')
    cnt <- 0
    flag <- 0
    for(i in 1:length(objs)){
      cl1 <- class(eval(parse(text=objs[i])))
      if(cl1[1] == 'list'){
        # if an object is not an empty list check if its elements are of type 'opal'
        list2check <- eval(parse(text=objs[i]))
        if(length(list2check) > 0){
          cl2 <- class(list2check[[1]])
          for(s in 1:length(cl2)){
            if(cl2[s] == 'opal'){
              cnt <- cnt + 1
              opalist[[cnt]] <- objs[i]
              flag <- 1
            }
          }
        }
      }
    }
    if(flag == 1){ 
      if(length(opalist) > 1){
         flag <- 2
         return(list("flag"=flag, "opals"=unlist(opalist), "opals.list"=unlist(opalist)))
      }else{
        pp <- opalist[[1]] 
        opals <- eval(parse(text=pp))
        return(list("flag"=flag, "opals"=opals, "opals.list"=unlist(opalist)))
      }
    }else{
      return(list("flag"=flag, "opals"=NULL, "opals.list"=NULL))
    }
  }else{
    return(list("flag"=flag, "opals"=NULL, "opals.list"=NULL))
  }
  
}
#getOpals