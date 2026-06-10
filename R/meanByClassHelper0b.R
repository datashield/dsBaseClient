#'
#' @title Runs the computation if variables are within a table structure
#' @description This is an internal function.
#' @details This function is called by the function 'ds.meanByClass' to produce the final tables
#' if the user specify a table structure.
#' @param x a character, the name of the dataset to get the subsets from.
#' @param outvar a character vector, the names of the continuous variables
#' @param covar a character vector, the names of up to 3 categorical variables
#' @param type a character which represents the type of analysis to carry out. If \code{type} is set to
#' 'combine', a pooled table of results is generated. If \code{type} is set to 'split', a table of results
#' is generated for each study.
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} objects obtained after login. If the <datasources>
#' the default set of connections will be used: see \link[DSI]{datashield.connections_default}.
#' @return a table or a list of tables that hold the length of the numeric variable(s) and their mean
#' and standard deviation in each subgroup (subset).
#' @keywords internal
#' @author Gaye, A.
#'
meanByClassHelper0b <- function(x, outvar, covar, type, datasources){
  if(is.null(outvar)){
    stop(" Please specify at least 1 continuous variables - see parameter 'outvar'!", call.=FALSE)
  }

  if(is.null(covar)){
    stop(" Please specify at 1 or up to 3 categorical variables (factors) - see parameter 'covar'!", call.=FALSE)
  }

  if(length(covar) > 3){
    stop("More than 3 categorical variables specified! - see parameter 'covar'.", call.=FALSE)
  }

  # categories in each of the categorical variables
  classes <- vector("list", length(covar))
  for(i in 1:length(covar)){
    cally <- paste0("levelsDS(",paste0(x, '$', covar[i]), ")")

    all.study.levels <- list()
    full.levels.resp <- DSI::datashield.aggregate(datasources, as.symbol(cally))
    for (index in 1:length(full.levels.resp)) {
      if (any(is.na(full.levels.resp[[i]]$Levels)))
        stop(paste0("Failed to get levels from study: ", full.levels.resp[[i]]$ValidityMessage), call.=FALSE)
      all.study.levels[[index]] <- full.levels.resp[[i]]$Levels
    }
    classes[[i]] <- all.study.levels
  }

  # loop through the datasources and break down the original dataset by the specified categorical variable
  # the names of the subset tables are stored for mean and sd computations
  message("Generating the required subset tables (this may take couple of minutes, please do not interrupt!)")
  subsetnames <- vector("list", length(datasources))
  for(i in 1:length(datasources)){
    message("--", names(datasources)[i])
    datasets <- x
    for(j in 1:length(covar)){
      message("  ", covar[j], "...")
      newnames <- meanByClassHelper1(datasources[i], datasets, covar[j], classes[[j]][[i]])
      datasets <- newnames
    }
    subsetnames[[i]] <- datasets
  }
  names(subsetnames) <- names(datasources)

  # a study might have invalid sub-datasets which we cannot get mean and sd from, to identify those
  # we loop by categories, if a study has invalid table (i.e. table with NAs only) we exclude it
  # for that category when calculating the mean and sd values for that category
  invalidrecorder <- vector("list", length(datasources))
  for(i in 1:length(datasources)){
    for(j in 1:length(subsetnames[[i]])){
      check1 <- which(unlist(strsplit(subsetnames[[i]][j],"_")) == "INVALID")
      check2 <- which(unlist(strsplit(subsetnames[[i]][j],"_")) == "EMPTY")
      if(length(check1) > 0 | length(check2 > 0)){
        invalidrecorder[[i]] <- append(invalidrecorder[[i]], 1)
      }else{
        invalidrecorder[[i]] <- append(invalidrecorder[[i]], 0)
      }
    }
  }

  # compute the length, mean and standard deviation for each 'outvar'
  if(type=='combine'){
    results <- meanByClassHelper2(datasources, subsetnames, outvar, invalidrecorder)
    return(results)
  }else{
    if(type=='split'){
      results <- meanByClassHelper3(datasources, subsetnames, outvar, invalidrecorder)
      return(results)
    }else{
      stop('Function argument "type" has to be either "combine" or "split"')
    }
  }

}
