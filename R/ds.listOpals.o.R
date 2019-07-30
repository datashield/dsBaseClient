#'
#' @title ds.listOpals list all Opal objects in the analytic environment
#' @description ds.listOpals calls the internal DataSHIELD function getOpals() which identifies
#' all Opal objects in the analytic environment.
#' @details ds.listOpals calls the internal DataSHIELD function getOpals() which identifies
#' all Opal objects in the analytic environment. If there are no Opal servers in the analytic
#' environment ds.listOpalsIreminds the user that they have to login to a valid set of Opal
#' login objects, if they wish to use DataSHIELD. If there is only one set of Opals, ds.listOpals
#' copies that one set and names the copy 'default.opals'. This default set will then be used by
#' default by all subsequent calls to client-side functions. If there is more than one set of Opals
#' in the analytic environment, ds.listOpals tells the user that they can either explicitly specify the
#' Opals to be used by each client-side function by providing an explicit "datasources=" argument
#' for each call, or can alternatively use the ds.setDefaultOpals function to specify a default
#' set of Opals to be used by all client-side calls unless over-ruled by the 'datasources=' argument.
#' @keywords internal
#' @return Lists all of the sets of Opals currently found in the analytic environment and advises
#' the user how best to respond depending whether there are zero, one or multiple Opals detected.
#' @author Burton, PR. 28/9/16
#' @export
#'

ds.listOpals.o<-function(){

  findLogin <- getOpals()
  message(paste0(" This function lists all Opal objects in the R analysis environment\n"))
  if (findLogin$flag == 0){
  stop(" Are you logged in to any server? Please provide a valid opal login object! ", call.=FALSE)
  }

  if(findLogin$flag==1){
    message(paste0(" There is only one set of opals available,\n that is: '",findLogin$opals.list,"'\n\n"))
    eval(parse(text=paste0("default.opals <- ",findLogin$opals.list)),envir=0)
    return(message(paste0(" This set of Opals has been copied to create 'default.opals',\n which all DataSHIELD functions will now use by default.\n If you want to change the default Opal object,\n please run the function ds.setDefaultOpals() again. \n\n")))
    }



  if (findLogin$flag > 1){
  message(paste0(" There is more than one set of opals available,\n these are: '", paste(findLogin$opals.list,collapse="', '"), "'!!"))
          return(message(paste0("\n\n You can either choose to specify the Opals you wish to use\n for each individual function call using the argument:\n 'datasources=name of opal object' [no inverted commas]\n or else use the ds.setDefaultOpals() function\n to create the object 'default.opals', which all\n DataSHIELD functions will then use by default.\n\n")))
   } else {
            stop("End of process: please enter a valid login object with no inverted commas", call.=FALSE)
          }
}
#ds.listOpals	  


