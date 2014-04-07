#' 
#' @title Generates results tables for each study separately
#' @description This is an internal function.
#' @details This function is called by the function 'ds.meanByClass' to produce the final tables
#' if the user sets the parmater 'type' to 'split'.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @param tablenames a character vector, the name of the subset tables
#' @param variables a character vector, the names of the continuous variables to computes a mean for. 
#' @return a list which one results table for each study.
#'
.meanByClassHelper3 <- function(datasources, tablenames, variables){
  
  finalist <- vector('list', length(datasources))
  for(s in 1:length(datasources)){
  
    # now get the mean and SD for the continuous variables in each of tthe subset tables
    finaltable <- matrix(numeric(0), ncol=length(tablenames))
    finalrows <- c()
    for(z in 1:length(variables)){
      # set an empty matrix to hold the results
      outable <- matrix(numeric(0), nrow=2, ncol=length(tablenames))
      xrows <- c(paste0(variables[z],'(length)'), paste0(variables[z],'(mean&sd)'))
      for(i in 1:length(tablenames)){
        ll <- unlist(ds.length(datasources[s], paste0(tablenames[i],'$',variables[z])))
        mm <- round(unlist(ds.mean(datasources[s], paste0(tablenames[i],'$',variables[z]))),2)
        sdv <- round(unlist(ds.var(datasources[s], paste0(tablenames[i],'$',variables[z]))),2)
        mean.sd <- paste0(mm, '(', round(sqrt(sdv),2), ')')
        entries <- c(ll, mean.sd)
        for(j in 1:2){
          outable[j,i] <-  entries[j]
        }
      }
      finalrows <- append(finalrows, xrows)
      finaltable <- rbind(finaltable, outable)
    }
    
    # specify the name of the rows and the columns
    colnames(finaltable) <- tablenames
    rownames (finaltable) <- finalrows
    
    finalist[[s]] <- finaltable
  
  }
  names(finalist) <- names(datasources)
  return(finalist)
}