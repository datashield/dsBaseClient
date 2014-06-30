#' 
#' @title Finalizes the output of the function \code{table1dDS}
#' @description this is an INTERNAL function required by the client function \code{ds.table1D}.
#' @details It finalizes the final output.
#' @param input a list object returned by the server side function \code{table1d.ds}
#' @keywords internal
#' @return a list that contains the elements returned by the client function \code{ds.table1d}
#' @author Burton, P.; Gaye, A.
#'
table1dhelper4  <- function(input) { 

  # last script required by the function ds.table1d
  
  # names of the studies
  opals.names <- names(input)
  
  # number of studies
  num.sources <- length(opals.names)
  
  # get some required input object using the function 'table1dhelper1'
  out1  <- table1dhelper1(input)
  opals.valid.binary  <- out1$opals.valid.binary
  zero.studies.valid  <- out1$zero.studies.valid 
  
  # get some required input object using the function 'table1dhelper2'
  out2 <- table1dhelper2(input)
  valid.output.matrix <- out2
  
  # simple summary of data from ALL studies (valid and invalid)
  study.validity <- rep(NA,num.sources)
  inp.obj <- input
  for(k in 1:num.sources)
  {
    study.validity[k] <- as.logical(inp.obj[[k]]$is.factor.valid)
  }
  final.output.list <- NULL
  for(m in 1:num.sources)
  {
    temp.list <- list(as.name(opals.names[[m]]),study.validity[m],as.table(inp.obj[[m]]$safe.table),
                    "************************************************")
    names(temp.list) <- c("study.name","is.table.valid","safe.table","******* STUDY COMPLETED *******")                     
    final.output.list <- c(final.output.list,temp.list)                 
  }  
  
  # get some required input object using the function 'table1dhelper3'
  out3  <- table1dhelper3(zero.studies.valid, valid.output.matrix, final.output.list)
  valid.counts.matrix <- out3$valid.counts.matrix
  valid.colperc.matrix <- out3$valid.colperc.matrix
  valid.rowperc.matrix <- out3$valid.rowperc.matrix
  valid.globalperc.matrix <- out3$valid.globalperc.matrix

  # only enact if at least one study valid
  if(zero.studies.valid==FALSE)
  {     
    invalid.study.output.obj <- NULL

    for(k in 1:length(opals.valid.binary))
    {
      if(opals.valid.binary[k]==0) {invalid.study.output.obj <- c(invalid.study.output.obj,k)}
    }

    # create flags if studies are valid or not
    if(is.null(invalid.study.output.obj))study.validity.flag <- "ALL STUDIES VALID"
    if(!is.null(invalid.study.output.obj))study.validity.flag <- paste("DATA INVALID IN",opals.names[invalid.study.output.obj])
                                                                     
    terminal.output.list <- list(final.output.list,as.matrix(valid.counts.matrix),
            as.matrix(valid.colperc.matrix),as.matrix(valid.rowperc.matrix),as.matrix(valid.globalperc.matrix),
            study.validity.flag)

    names(terminal.output.list)[[1]] <- "OPALS.DATA.OVERVIEW"
    names(terminal.output.list)[[2]] <- "TABLE.VALID.DATA.COUNTS"
    names(terminal.output.list)[[3]] <- "TABLE.VALID.DATA.COLUMN.PERCENTS"
    names(terminal.output.list)[[4]] <- "TABLE.VALID.DATA.ROW.PERCENTS"
    names(terminal.output.list)[[5]] <- "TABLE.VALID.DATA.GLOBAL.PERCENTS"
    names(terminal.output.list)[[6]] <- "VALIDITY.WARNING"

  }else{
    study.validity.flag <- "NO STUDIES HAVE VALID DATA - SO NO ATTEMPT TO COMBINE"
    terminal.output.list <- list(final.output.list,study.validity.flag)
    names(terminal.output.list)[[1]] <- "OPALS.DATA.OVERVIEW"
    names(terminal.output.list)[[2]] <- "VALIDITY.WARNING"
  } 
  return <- terminal.output.list
}
