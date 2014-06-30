#' 
#' @title Combines output of server side function \code{table1d.ds}
#' @description this is an INTERNAL function required by the client function \code{ds.table1D}. 
#' @details It gathers the objects returned by \code{table1dDS}
#' @param input a list object returned by the server side function \code{table1d.ds}
#' @keywords internal
#' @return a matrix containing study specific and overall (combined) tables of counts based ONLY
#' on data from studies where all table categories are VALID.
#' @author Burton, P.; Gaye, A.
#'
table1dhelper2  <- function (input){ 
  
  # Second script required by the function ds.table1d

  # call the function that carries out validity checks
  validity  <- table1dhelper1(input)
  
  # assign the objects returned by the function 'table1dhelper1'                                                                             
  num.opals  <- validity$num.opals
  num.valid.tables  <- validity$num.valid.tables
  opals.valid.binary  <- validity$opals.valid.binary
  opals.valid.id  <- validity$opals.valid.id
  zero.studies.valid  <- validity$zero.studies.valid

  # name of the studies
  opals.names <- names(input)
  
  # assign the input object - output from the server side function 'table1d.ds'
  inp.obj   <- input
  
  # if none of the studies is valid set the 'valid.output.matrix' to NULL
  if(num.valid.tables==0){valid.output.matrix  <- NULL}

  # only enact if at least one study valid
  if(zero.studies.valid==FALSE)
  {
    # identify the full set of unique categories across all studies
    all.categories.vector <- NULL

    for(j in 1:num.opals)
    {
      if(opals.valid.binary[j]==1)
      {    
        all.categories.vector <- c(all.categories.vector,names(as.table(inp.obj[[j]]$safe.table)))
      }
    }
    unique.categories <- sort(unique(all.categories.vector))
    num.cats <- length(unique.categories)

    # create valid.output.matrix with columns for valid studies and rows for unique categories
    valid.output.matrix <- matrix(0,nrow=num.cats,ncol=num.valid.tables)
    sum.counts <- as.integer(rep(0,num.cats))

    for(j in 1:num.valid.tables)
      {
        vj <- opals.valid.id[j]
        counts.vector <- as.integer(inp.obj[[vj]]$safe.table)
        categories.vector <- names(as.table(inp.obj[[vj]]$safe.table))
       
        for(k in 1:length(counts.vector))
        {
          # work through categories in each study one at a time
          current.cat <- categories.vector[k]
          current.count <- counts.vector[k]
          
          for(m in 1:num.cats)
          {        
            #run through all unique categories to decide where to add count
            if(current.cat==unique.categories[m]) valid.output.matrix[m,j] <- current.count
          }
        }
      } 
    dimnames(valid.output.matrix)[1] <- list(unique.categories)
    dimnames(valid.output.matrix)[2] <- list(opals.names[opals.valid.id])

  # returns the valid matrix
  return (valid.output.matrix)
  }else{
    
  }
} 


