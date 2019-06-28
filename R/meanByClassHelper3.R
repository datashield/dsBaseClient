#' 
#' @title Generates results tables for each study separately
#' @description This is an internal function.
#' @details This function is called by the function 'ds.meanByClass' to produce the final tables
#' if the user sets the parmater 'type' to 'split'.
#' @param dtsources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @param tablenames a character vector, the name of the subset tables
#' @param variables a character vector, the names of the continuous variables to computes a mean for. 
#' @param invalidrecorder a list, holds informations about invalid subsets in each study
#' @keywords internal
#' @return a list which one results table for each study.
#' @author Gaye, A.
#'
meanByClassHelper3 <- function(dtsources, tablenames, variables, invalidrecorder){
  numtables <- length(tablenames[[1]])
  stdnames <- names(dtsources)
  
  finalist <- vector('list', length(dtsources))
  for(s in 1:length(dtsources)){
  
    # now get the mean and SD for the continuous variables in each of tthe subset tables
    finaltable <- matrix(numeric(0), ncol=numtables)
    finalrows <- c()
    for(z in 1:length(variables)){
      # set an empty matrix to hold the results
      outable <- matrix(numeric(0), nrow=2, ncol=numtables)
      xrows <- c(paste0(variables[z],'(length)'), paste0(variables[z],'(mean&sd)'))
      for(i in 1:numtables){
        # inform of progress
        message(paste0(stdnames[s], ", ", variables[z], " - Processing subset table ", i, " of ", numtables, "..."))
        
        # check what datasource has invalid subset
        rc <- c()
        if(invalidrecorder[[s]][i] == 1){ rc <- append(rc, q) }
        
        if(length(rc) > 0){
          cally <- paste0("length(", paste0(tablenames[[s]][i],'$',variables[z]), ")")
          ll <- unlist(opal::datashield.aggregate(dtsources[s], as.symbol(cally)))
          mm <- NA
          sdv <- NA
          mean.sd <- paste0(mm, '(', sdv, ')')
          entries <- c(ll, mean.sd)
        }else{
          cally <- paste0("length(", paste0(tablenames[[s]][i],'$',variables[z]), ")")
          ll <- unlist(opal::datashield.aggregate(dtsources[s], as.symbol(cally)))
          mm <- round(getPooledMean(dtsources[s], paste0(tablenames[[s]][i],'$',variables[z])),2)
          sdv <- round(getPooledVar(dtsources[s], paste0(tablenames[[s]][i],'$',variables[z])),2)
          if(is.na(mm)){ sdv <- NA }
          mean.sd <- paste0(mm, '(', round(sqrt(sdv),2), ')')
          entries <- c(ll, mean.sd)
        }
        for(j in 1:2){
          outable[j,i] <-  entries[j]
        }
      }
      finalrows <- append(finalrows, xrows)
      finaltable <- rbind(finaltable, outable)
    }
    
    # specify the name of the rows and the columns
    cols <- tablenames[[s]]
    for(i in which(invalidrecorder[[s]] == 1)){
      cols[i] <- paste(unlist(strsplit(cols[i], "_INVALID")), collapse="")
    }
    colnames(finaltable) <- cols
    rownames (finaltable) <- finalrows
    
    finalist[[s]] <- finaltable
  
  }
  names(finalist) <- names(dtsources)
  return(finalist)
}
