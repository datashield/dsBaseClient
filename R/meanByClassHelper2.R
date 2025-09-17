#'
#' @title Generates a table for pooled results
#' @description This is an internal function.
#' @details This function is called by the function 'ds.meanByClass' to produce the final table
#' if the user sets the parameter 'type' to combine (the default behaviour of 'ds.meanByClass').
#' @param dtsources a list of \code{\link[DSI]{DSConnection-class}} objects obtained after login. If the <datasources>
#' the default set of connections will be used: see \link[DSI]{datashield.connections_default}.
#' @param tablenames a character vector, the name of the subset tables
#' @param variables a character vector, the names of the continuous variables to computes a mean for.
#' @param invalidrecorder a list, holds information about invalid subsets in each study.
#' @keywords internal
#' @return a matrix, a table which contains the length, mean and standard deviation of each of the
#' specified 'variables' in each subset table.
#' @author Gaye, A.
#'
meanByClassHelper2 <- function(dtsources, tablenames, variables, invalidrecorder){
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
      tnames <- tablenames
      tablename <- paste(unlist(strsplit(tablenames[[1]][i], "_INVALID")), collapse="")
      tablename <- paste(unlist(strsplit(tablename, "_EMPTY")), collapse="")
      # check what datasource has invalid subset
      rc <- c()
      for(q in 1:length(dtsources)){
        if(invalidrecorder[[q]][i] == 1){
          rc <- append(rc, q)
        }
      }
      if(length(rc) > 0){
        lengths <- c()
        for(qq in 1:length(dtsources)){
          # check if the subset table exists (when the initial table is invalid no subsequent subset is created)
          cally <- call("exists", tnames[[qq]][i])
          def <-  unlist(DSI::datashield.aggregate(dtsources[qq], cally))
          if(def){
            cally <- call("dimDS", tnames[[qq]][i])
            temp <- unlist(DSI::datashield.aggregate(dtsources[qq], cally))
            lengths <- append(lengths, temp[1])
          }else{
            lengths <- append(lengths, 0)
          }
        }
        if(length(rc) == length(dtsources)){
          ll <- sum(lengths)
          mm <- NA
          sdv <- NA
          mean.sd <- paste0(mm, '(', sdv, ')')
          entries <- c(ll, mean.sd)
        }else{
          ll <- sum(lengths)
          dtsc <- dtsources[-rc] # ignore invalid tables when it comes to mean and sd calculation, there value is NA anyway
          mm <- round(getPooledMean(dtsc, paste0(tablename,'$',variables[z])),2)
          sdv <- round(getPooledVar(dtsc, paste0(tablename,'$',variables[z])),2)
          if(is.na(mm)){ sdv <- NA }
          mean.sd <- paste0(mm, '(', round(sqrt(sdv),2), ')')
          entries <- c(ll, mean.sd)
        }
      }else{
        cally <- call("lengthDS", paste0(tablename,'$',variables[z]))
        lengths <- DSI::datashield.aggregate(dtsources, cally)
        ll <- sum(unlist(lengths))
        mm <- round(getPooledMean(dtsources, paste0(tablename,'$',variables[z])),2)
        sdv <- round(getPooledVar(dtsources, paste0(tablename,'$',variables[z])),2)
        if(is.na(mm)){ sdv <- NA}
        mean.sd <- paste0(mm, '(', round(sqrt(sdv),2), ')')
        entries <- c(ll, mean.sd)
      }
      for(j in 1:2){
        outable[j,i] <- entries[j]
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
