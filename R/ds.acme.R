#'
#' @title ds.acme function template
#' @description example aggregate function template
#' @details Client-side acme example aggregate function template
#' This file is to be placed in server-sides R directory.
#' @param name a character vector containing "a name"
#' @param datasources specifies the particular 'connection object(s)' to use.
#' @return character vector containing "a message"
#' @author Ever, Who.
#' @export
#'

ds.acme <- function(model, Set, type.data = "SummarizedExperiment", threshold = NULL, 
                    reference = NULL, annotCols=NULL, datasources=NULL)
{   
    # if no opal login details are provided look for 'opal' objects in the environment
    if (is.null(datasources))
    {
        datasources <-  DSI::datashield.connections_find()
    }
    type <- charmatch(type.data, c("SummarizedExperiment", "matrix", "htseq"))
  
    if(is.na(type))
    {
       stop("type.data must be 'SummarizedExperiment', 'matrix', 'htseq'")
    }
    
    mt <- all.vars(model)
    variable_names <- mt[1] 
    
    if (length(mt)>1)
    {
      covariable_names <- paste(mt[-1], collapse=",")
    }
    
    else
    {
      covariable_names <- NULL
    }
    
    if (!is.null(annotCols))
    {
      annotCols <- paste(annotCols, collapse=",")
    }

    # call the server side function
    calltext <- call("acmeDS",Set, type,variable_names, covariable_names, annotCols, threshold, reference)

    output <- datashield.aggregate(datasources, calltext)
  
    return(output)
}
#ds.acme
