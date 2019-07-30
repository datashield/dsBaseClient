#' 
#' @title Retrieves the dimension of an object
#' @description this function is similar to R function \code{dim}
#' @details the function returns the unpooled or pooled dimension of the object by summing
#' up the individual dimensions returned from each study or the dimension of the object in each 
#' study. Unlike the other DataSHIELD function the default behaviour is to output the dimension 
#' of each study separately. 
#' @param x a character, the name of R table object, for example a matrix, array or data frame
#' @param type a character which represents the type of analysis to carry out. 
#' If \code{type} is set to 'combine', 'combined', 'combines' or 'c', the global dimension is returned 
#' if \code{type} is set to 'split', 'splits' or 's', the dimension is returned separately for each study.
#' if \code{type} is set to 'both' or 'b', both sets of outputs are produced
#' @param checks a Boolean indicator of whether to undertake optional checks of model
#' components. Defaults to checks=FALSE to save time. It is suggested that checks
#' should only be undertaken once the function call has failed
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @return for an array, \code{NULL} or a vector of mode \code{integer}
#' @author Amadou Gaye, Julia Isaeva, Demetris Avraam, for DataSHIELD Development Team
#' @seealso \link{ds.dataframe} to generate a table of type dataframe.
#' @seealso \link{ds.changeRefGroup} to change the reference level of a factor.
#' @seealso \link{ds.colnames} to obtain the column names of a matrix or a data frame
#' @seealso \link{ds.asMatrix} to coerce an object into a matrix type.
#' @seealso \link{ds.length} to obtain the size of a vector.
#' @export
#' @examples
#' \dontrun{
#' 
#'   # load that contains the login details
#'   data(logindata)
#' 
#'   # login and assign all the stored variables.
#'   opals <- datashield.login(logins=logindata,assign=TRUE)
#' 
#'   # Example 1: Get the dimension of the assigned datasets in each study
#'   ds.dim(x='D', type='combine')
#' 
#'   # Example 2: Get the pooled dimension of the assigned datasets
#'   ds.dim(x='D', type='combine')
#'
#'   # Example 3: Get the dimension og the datasets in each single study
#'   # and the pooled dimension  - default
#'   ds.dim(x='D') 
#' 
#'   # Example 4: Input has to be either matrix, data frame or an array
#'   # In the below example, the inpout is a vector so it will not work.
#'   ds.dim(x='D$LAB_TSC')
#' 
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(opals)
#' 
#' }
#' 
ds.dim.o <- function(x=NULL, type='both', checks=FALSE, datasources=NULL) {
  
  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }
  
  if(is.null(x)){
    stop("Please provide a the name of a data.frame or matrix!", call.=FALSE)
  }

  ########################################################################################################
  # MODULE: GENERIC OPTIONAL CHECKS TO ENSURE CONSISTENT STRUCTURE OF KEY VARIABLES IN DIFFERENT SOURCES #
  # beginning of optional checks - the process stops and reports as soon as one check fails              #
  #                                                                                                      #
  if(checks){                                                                                            #
    message(" -- Verifying the variables in the model")                                                  #
    # check if the input object(s) is(are) defined in all the studies                                    #
    defined <- isDefined(datasources, x)                                                                 #
    # throw a message and stop if input is not table structure                                           #
    if(! defined){                                                                                       #
      stop("The input object is not defined in all studies!", call.=FALSE)                               #
    }                                                                                                    #
    # call the internal function that checks the input object is suitable in all studies                 #
    typ <- checkClass(datasources, x)                                                                    #
    # throw a message and stop if input is not table structure                                           #
    if(typ != 'data.frame' & typ!= 'matrix'){                                                            #
      stop("The input object must be a table structure!", call.=FALSE)                                   #
    }                                                                                                    #
  }                                                                                                      #
  ########################################################################################################
  
  
  ###################################################################################################
  #MODULE: EXTEND "type" argument to include "both" and enable valid alisases                       #
  if(type == 'combine' | type == 'combined' | type == 'combines' | type == 'c') type <- 'combine'   #
  if(type == 'split' | type == 'splits' | type == 's') type <- 'split'                              #
  if(type == 'both' | type == 'b' ) type <- 'both'                                                  #
  #
  #MODIFY FUNCTION CODE TO DEAL WITH ALL THREE TYPES                                                #
  ###################################################################################################
  
  cally <- paste0("dimDS.o(", x, ")")
  dimensions <- opal::datashield.aggregate(datasources, as.symbol(cally))
  
  # names of the studies to be used in the output
  stdnames <- names(datasources)
  outputnames <- c()
  for (i in 1:length(datasources)){
    outputnames[i] <- paste0('dimensions of ', x, ' in ', stdnames[i])
  }
  
  # find the dimensions of the combined dataframe or matrix
  global.dim1 <- 0
  global.dim2 <- dimensions[[1]][2]
  for(i in 1:length(datasources)){
    global.dim1 <- global.dim1 + dimensions[[i]][1]
  }
  pooled.dim <- list(c(global.dim1, global.dim2))
  
  if(type=="combine"){
    out <- pooled.dim
	names(out) <- paste0('dimensions of ', x, ' in combined studies')
  }else{
    if(type=="split"){
	  out <- dimensions
	  names(out) <- outputnames
    }else{
	  if(type=="both"){     
        out <- c(dimensions, pooled.dim)
		names(out) <- c(outputnames, paste0('dimensions of ', x, ' in combined studies'))
	  }else{
        stop('Function argument "type" has to be either "both", "combine" or "split"')
      } 
    }
  }	
  
  return(out)
  
}
#ds.dim.o
