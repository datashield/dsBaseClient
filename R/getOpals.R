#' 
#' @title Gets the opal objects
#' @description This is an internal function.
#' @details The function searches for a list containing object of type 'opal'
#' in the global environment and the testing environment; if more than one list is found it return the lastest.
#' This way no matter what the user calls his opal login object it will be captured. 
#' @keywords internal
#' @return a list of opal object obtained after login into the servers
#' @author Gaye A, edited by Ryser-Welch P. 
#'
getOpals <- function()
{
  flag        <- 0;
  opal.list   <- NULL
  return.list <- list("flag"=flag, "opals"=NULL, "opals.list"=NULL)

  curr.ds.test_env <- NULL
  try(curr.ds.test_env <- get("ds.test_env", envir = .GlobalEnv), silent = TRUE)

  if (! is.null(curr.ds.test_env))
  {
    opal.list <- init.object.list.testing.environment(ls(curr.ds.test_env))
  }
  else
  {
    opal.list <- init.object.list.global.environment(ls(.GlobalEnv))
  }
  
  return.list <- init.opal.list(opal.list)
#  print("return.list")
#  print(return.list)

  return(return.list)
}
  
init.object.list.testing.environment <- function(objs)
{
  opalist <- vector('list')
  counter <- 0 
  for(i in 1:length(objs))
  {
     if (objs[i] == "connection.opal")
     {
        counter <- counter + 1
        opalist[[counter]] <- paste("ds.test_env$",objs[1],sep="")
     }
  }
  return(opalist)
}
  

init.object.list.global.environment <- function(objs)
{
  opalist <- vector('list')
  counter <- 0
  for(i in 1:length(objs))
  {
    class.element.name = class(eval(parse(text=objs[i])))
    if(class.element.name[1] == 'list')
    {
      list2check <- eval(parse(text=objs[i]))
      if(length(list2check) > 0)
      {
        cl2 <- class(list2check[[1]])
        for(s in 1:length(cl2))
        {
          if(cl2[s] == 'opal')
          {
            counter <- counter + 1
            opalist[[counter]] <- objs[i]
          }
        }
      }
    }
  }
#  print(opalist)
  return(opalist)
}

init.opal.list <- function(opal.list)
{
     if(length(opal.list) > 1)
     {
        flag <- 2
        return(list("flag"=flag, "opals"=unlist(opal.list), "opals.list"=unlist(opal.list)))
     }
     else
     {
        flag <- 1
        return(list("flag"=flag, "opals"=eval(parse(text=opal.list[[1]])), "opals.list"=unlist(opal.list)))
     }
 }
#getOpal
