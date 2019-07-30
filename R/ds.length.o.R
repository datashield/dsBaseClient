#' 
#' @title Gets the length of a vector or list
#' @description This function is similar to R function \code{length}.
#' @details The function returns the pooled length of the a vector or a list, 
#' or the length of the a vector or a list for each study separately.
#' @param x a string character, the name of a vector or list
#' @param type a character which represents the type of analysis to carry out. 
#' If \code{type} is set to 'combine', 'combined', 'combines' or 'c', a global length is returned 
#' if \code{type} is set to 'split', 'splits' or 's', the length is returned separately for each study.
#' if \code{type} is set to 'both' or 'b', both sets of outputs are produced
#' @param checks a Boolean indicator of whether to undertake optional checks of model
#' components. Defaults to checks=FALSE to save time. It is suggested that checks
#' should only be undertaken once the function call has failed
#' @param datasources a list of opal object(s) obtained after login in to opal
#' servers; these objects hold also the data assign to R, as \code{dataframe},
#' from opal datasources.
#' @return a numeric, the length of the input vector or list.
#' @author Amadou Gaye, Demetris Avraam, for DataSHIELD Development Team
#' @export
#' @examples
#' \dontrun{
#' 
#'   # load that contains the login details
#'   data(logindata)
#' 
#'   # login and assign all the variables stored on the server side
#'   opals <- datashield.login(logins=logindata, assign=TRUE)
#' 
#'   # Example 1: Get the total number of observations of the vector of
#'   # variable 'LAB_TSC' across all the studies 
#'   ds.length(x='D$LAB_TSC', type='combine')
#' 
#'   # Example 2: Get the number of observations of the vector of variable
#'   # 'LAB_TSC' for each study separately
#'   ds.length(x='D$LAB_TSC', type='split')
#'
#'   # Example 3: Get the number of observations on each study and the total
#'   # number of observations across all the studies for the variable 'LAB_TSC'
#'   ds.length(x='D$LAB_TSC', type='both')
#' 
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(opals) 
#' 
#' }
#' 
ds.length.o = function(x=NULL, type='both', checks='FALSE', datasources=NULL){
  
  #####################################################################################
  #MODULE 1: IDENTIFY DEFAULT OPALS                                                   #
  # if no opal login details are provided look for 'opal' objects in the environment  #
  if(is.null(datasources)){                                                           #
    datasources <- findLoginObjects()                                                 #
  }                                                                                   #
  ##################################################################################### 
  
  #####################################################################################
  #MODULE 2: SET UP KEY VARIABLES ALLOWING FOR DIFFERENT INPUT FORMATS                #
  if(is.null(x)){                                                                   #
    stop("Please provide the name of the input vector!", call.=FALSE)               #
  }                                                                                 #
  # the input variable might be given as a variable in a data frame (i.e. D$x)      #
  # or just as a vector not attached to a table (i.e. x)                            #
  # we have to make sure the function deals with each case                          #
  xnames <- extract(x)                                                              #
  varname <- xnames$elements                                                        #
  obj2lookfor <- xnames$holders                                                     #
  #####################################################################################
  
  
  ###############################################################################################
  #MODULE 3: GENERIC OPTIONAL CHECKS TO ENSURE CONSISTENT STRUCTURE OF KEY VARIABLES            #
  #IN DIFFERENT SOURCES                                                                         #
  # beginning of optional checks - the process stops and reports as soon as one               #
  #check fails                                                                                #
  #
  if(checks){                                                                                 #
    message(" -- Verifying the variables in the model")                                       #
    #
    # check if the input object(s) is(are) defined in all the studies                           #
    if(is.na(obj2lookfor)){                                                                     #
      defined <- isDefined(datasources, varname)                                                #
    }else{                                                                                      #
      defined <- isDefined(datasources, obj2lookfor)                                            #
    }                                                                                           #
    #
    # call the internal function that checks the input object is suitable in all studies        #
    typ <- checkClass(datasources, x)                                                      #
    # the input object must be a vector or a list
    if(!('character' %in% typ) & !('factor' %in% typ) & !('integer' %in% typ) & !('logical' %in% typ) & !('numeric' %in% typ) & !('list' %in% typ)){
      stop("The input object must be a character, factor, integer, logical or numeric vector or a list.", call.=FALSE)
    }                                                                                         #
  }                                                                                             #
  ###############################################################################################
  
  ###################################################################################################
  #MODULE 4: EXTEND "type" argument to include "both" and enable valid alisases                     #
  if(type == 'combine' | type == 'combined' | type == 'combines' | type == 'c') type <- 'combine'   #
  if(type == 'split' | type == 'splits' | type == 's') type <- 'split'                              #
  if(type == 'both' | type == 'b' ) type <- 'both'                                                  #
  if(type != 'combine' & type != 'split' & type != 'both')                                          #
    stop('Function argument "type" has to be either "both", "combine" or "split"', call.=FALSE)     #
                                                                                                    #
  #MODIFY FUNCTION CODE TO DEAL WITH ALL THREE TYPES                                                #
  ###################################################################################################
  
  cally <- paste0("lengthDS.o(", x, ")")
  lengths <- opal::datashield.aggregate(datasources, as.symbol(cally))
  
  # names of the studies to be used in the output
  stdnames <- names(datasources)
  outputnames <- c()
  for (i in 1:length(datasources)){
    outputnames[i] <- paste0('length of ', x, ' in ', stdnames[i])
  }
  
  # calculate the combined length of the vector from all studies
  pooled.length <- list(sum(unlist(lengths)))

  if(type=="combine"){
    out <- pooled.length
    names(out) <- paste0('total length of ', x, ' in all studies combined')  
  }else{
    if(type=="split"){
	  out <- lengths
      names(out) <- outputnames
    }else{
      if(type=="both"){     
        out <- c(lengths, pooled.length)
        names(out) <- c(outputnames, paste0('total length of ', x, ' in all studies combined'))
      }else{
        stop('Function argument "type" has to be either "both", "combine" or "split"')
      } 
    }
  }	
  
  return(out)
  
}
#ds.length.o
