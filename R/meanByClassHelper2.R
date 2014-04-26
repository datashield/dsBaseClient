#' 
#' @title Generates a table for pooled results
#' @description This is an internal function.
#' @details This function is called by the function 'ds.meanByClass' to produce the final table
#' if the user sets the parmater 'type' to combine (the default behaviour of 'ds.meanByClass')
#' @param dtsources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @param tablenames a character vector, the name of the subset tables
#' @param variables a character vector, the names of the continuous variables to computes a mean for. 
#' @param invalidrecorder a list, holds informations about invalid subsets in each study.
#' @return a matrix, a table which contains the length, mean and standard deviation of each of the
#' specified 'variables' in each subset table.
#'
.meanByClassHelper2 <- function(dtsources, tablenames, variables, invalidrecorder){
  numtables <- length(tablenames[[1]])
  
  # now get the mean and SD for the continuous variables in each of the subset tables
  finaltable <- matrix(numeric(0), ncol=numtables)
  finalrows <- c()
  for(z in 1:length(variables)){
    # set an empty matrix to hold the results
    outable <- matrix(numeric(0), nrow=2, ncol=numtables)
    xrows <- c(paste0(variables[z],'(length)'), paste0(variables[z],'(mean&sd)'))
    for(i in 1:numtables){
      # inform of progress
      message(paste0(variables[z], " - Processing subset table ", i, " of ", numtables, "..."))
      
      tablename <- paste(unlist(strsplit(tablenames[[1]][i], "_INVALID")), collapse="")
      
      # check what datasource has invalid subset
      rc <- c()
      for(q in 1:length(dtsources)){
        if(invalidrecorder[[q]][i] == 1){ 
          rc <- append(rc, q) 
        }
      }
      if(length(rc) > 0){
        if(length(rc) == length(dtsources)){
          ll <- NA
          mm <- NA
          sdv <- NA
          mean.sd <- paste0(mm, '(', sdv, ')')
          entries <- c(ll, mean.sd)
        }else{
          dtsc <- dtsources[-rc]
          ll <- unlist(ds.length(dtsc, paste0(tablename,'$',variables[z])))
          mm <- round(unlist(ds.mean(dtsc, paste0(tablename,'$',variables[z]))),2)
          sdv <- round(unlist(ds.var(dtsc, paste0(tablename,'$',variables[z]))),2)
          if(is.na(mm)){ sdv <- NA }
          mean.sd <- paste0(mm, '(', round(sqrt(sdv),2), ')')
          entries <- c(ll, mean.sd)
        }
      }else{
        ll <- unlist(ds.length(dtsources, paste0(tablename,'$',variables[z])))
        mm <- round(unlist(ds.mean(dtsources, paste0(tablename,'$',variables[z]))),2)
        sdv <- round(unlist(ds.var(dtsources, paste0(tablename,'$',variables[z]))),2)
        if(is.na(mm)){ sdv <- NA}
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
  cols <- tablenames[[1]]
  for(i in which(invalidrecorder[[1]] == 1)){
    cols[i] <- paste(unlist(strsplit(cols[i], "_INVALID")), collapse="")
  }
  colnames(finaltable) <- cols
  rownames (finaltable) <- finalrows

  return(finaltable)
}