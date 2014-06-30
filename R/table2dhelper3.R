#' @title Generates percents from counts returned by the function \code{table2d.ds}
#' @description This is an INTERNAL function.
#' @details The function is required required by the client function \code{ds.table2D}.
#' @param input a list object returned by the server side function \code{table2dDS}
#' @param valid.output.array a valid array
#' @param zero.studies.valid a boolean indicating if all studies were not valid
#' @param unique.categories.1 a vector, the full set of unique categories across all studies (from valid binaries)
#' @param unique.categories.2 a vector, the full set of unique categories across all studies (from valid binaries) 
#' @param num.cats.1 length of the vector 'unique.categories.1'
#' @param num.cats.2 length of the vector 'unique.categories.2'
#' @param opals.valid.id a vector that gives the indices of the studies with valid tabulated vectors
#' @param num.valid.tables number of studies with valid tabulated vectors (no cell with count > 0 and < 5)
#' @keywords internal
#' @return a list that contains summary tables with columns and rows percents
#' @author Burton, P.; Gaye, A.
#'
table2dhelper3 <- function(input,valid.output.array,zero.studies.valid,unique.categories.1,unique.categories.2,
                           num.cats.1,num.cats.2,opals.valid.id,num.valid.tables){
 
  # simple summary of data from ALL studies (valid and invalid)
  opals.names <- names(input)
  num.opals <- length(input)
  study.validity <- rep(NA,num.opals)

  for(k in 1:num.opals)
  {
    study.validity[k] <- as.logical(input[[k]]$is.table.valid)
  }

  final.output.list <- NULL
  for(m in 1:num.opals)
  {
    temp.list <- list(as.name(opals.names[[m]]),study.validity[m],as.table(input[[m]]$safe.table),
                    "************************************************")
    names(temp.list) <- c("study.name","is.table.valid","safe.table","******* STUDY COMPLETED *******")                
    final.output.list <- c(final.output.list,temp.list)            
  }

  # only enact if at least one study is valid
  if(zero.studies.valid==FALSE){
        
    valid.counts.array <- array(NA,dim=c((num.cats.1+1),(num.cats.2+1),(num.valid.tables+1)))
    dimnames(valid.counts.array) <- list(c(unique.categories.1,"TOTAL"),c(unique.categories.2,"TOTAL"),
                                         c(opals.names[opals.valid.id],"ALL VALID STUDIES COMBINED"))
    valid.colperc.array <- valid.counts.array
    valid.rowperc.array <- valid.counts.array
    valid.globalperc.array <- valid.counts.array
    valid.counts.array[(1:num.cats.1),(1:num.cats.2),(1:(num.valid.tables+1))] <- valid.output.array
    num.tables <- dim(valid.counts.array)[3]
      
    for(j in 1:num.tables)
    {
      for(k in 1:num.cats.1)
      {
        valid.counts.array[k,(num.cats.2+1),j] <- sum(valid.counts.array[k,(1:num.cats.2),j])
      }

      for(m in 1:(num.cats.2+1))
      {
        valid.counts.array[(num.cats.1+1),m,j] <- sum(valid.counts.array[(1:num.cats.1),m,j])
      }
    } 

    for(j in 1:num.tables)
    {
      for(k in 1:(num.cats.1+1))
      {
        valid.rowperc.array[k,(1:(num.cats.2+1)),j] <- round( (100*valid.counts.array[k,(1:(num.cats.2+1)),j]/valid.counts.array[k,(num.cats.2+1),j]),2)
      }
    
      for(m in 1:(num.cats.2+1))
      {
        valid.colperc.array[(1:(num.cats.1+1)),m,j] <- round( (100*valid.counts.array[(1:(num.cats.1+1)),m,j]/valid.counts.array[(num.cats.1+1),m,j]),2)
      }
      
    }
     
     
    for(j in 1:num.tables)
    {
      for(k in 1:(num.cats.1+1))
      { 
        {
          for(m in 1:(num.cats.2+1))
            {
            valid.globalperc.array[k,m,j] <- round( (100*valid.counts.array[k,m,j]/valid.counts.array[(num.cats.1+1),(num.cats.2+1),j]),2)
            }
        }
      }
    }
   
    # return 4 key output objects
    obj2return <- list(final.output.list, valid.counts.array, valid.rowperc.array, valid.colperc.array, valid.globalperc.array)
    names(obj2return) <- c("final.output.list", "valid.counts.array", "valid.rowperc.array", "valid.colperc.array", "valid.globalperc.array")
    return(obj2return)
    
  }else{
    # return only 'final.output.list'
    obj2return <- list(final.output.list)
    names(obj2return) <- c("final.output.list")
    return(obj2return)    
  }
} 
