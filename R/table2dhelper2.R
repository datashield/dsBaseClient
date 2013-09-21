#'
#' @title Generates a valid array
#' @description this is an INTERNAL function required by the client function
#' \code{ds.table2d}. The function is not 'exported' and hence not visible to user and
#' is only called by \code{ds.table2d}
#' @param input a list object returned by the server side function \code{table2d.ds}
#' @param num.valid.tables number of studies with valid tabulated vectors (no cell with count > 0 and < 5)
#' @param opals.valid.binary vector of binaries indicating whether or not the tabulate vector
#' was valid or not (0 = not valid and 1 = not valid)
#' @param opals.valid.id a vector that gives the indices of the studies with valid tabulated vectors
#' @param zero.studies.valid a boolean indicating if all studies were not valid
#' @return a list which contains a valid array and vectors of unique categories
#' @author Burton, P.; Gaye, A.
#'
table2dhelper2 <- function(input, num.valid.tables, opals.valid.binary, opals.valid.id, zero.studies.valid){

  num.opals <- length(input)
  opals.names <- names(input)
  
  if(num.valid.tables==0){valid.output.array <- NULL}
  if(num.valid.tables==0){valid.counts.array <- NULL}
  if(num.valid.tables==0){valid.colperc.array <- NULL}
  if(num.valid.tables==0){valid.rowperc.array <- NULL}
  if(num.valid.tables==0){valid.globalperc.array <- NULL}

  # only enact if at least one study valid
  if(zero.studies.valid==FALSE)
  { 
    # identify the full set of unique categories across all studies
    all.categories.vector.1 <- NULL
    all.categories.vector.2 <- NULL
    for(j in 1:num.opals)
    {
      if(opals.valid.binary[j]==1)
      {    
        all.categories.vector.1 <- c(all.categories.vector.1,dimnames(as.table(input[[j]]$safe.table))[[1]])
        all.categories.vector.2 <- c(all.categories.vector.2,dimnames(as.table(input[[j]]$safe.table))[[2]])
      }
    }
    unique.categories.1 <- sort(unique(all.categories.vector.1))
    unique.categories.2 <- sort(unique(all.categories.vector.2))
    num.cats.1 <- length(unique.categories.1)
    num.cats.2 <- length(unique.categories.2)

    # create valid.output.matrix with columns for valid studies and rows for unique categories
    valid.output.array <- array(0,dim=c(num.cats.1,num.cats.2,(num.valid.tables+1)))
    for(j in 1:num.valid.tables)
    {
      vj <- opals.valid.id[j]
      counts.matrix <- as.matrix(input[[vj]]$safe.table)
      categories.vector.1 <- dimnames(as.table(input[[vj]]$safe.table))[[1]]
      categories.vector.2 <- dimnames(as.table(input[[vj]]$safe.table))[[2]]
      for(g in 1:length(categories.vector.1))
      {
        for(h in 1:length(categories.vector.2))
        {
            # work through categories in each study one at a time
            current.row <- categories.vector.1[g]
            current.col <- categories.vector.2[h]      
            current.count <- counts.matrix[g,h]
            for(m in 1:num.cats.1)
            {
              for(n in 1:num.cats.2)
              {
                  if(unique.categories.1[m]==current.row && unique.categories.2[n]==current.col){
                     valid.output.array[m,n,j] <- current.count
                  }
              }
            }
          }
        }
        valid.output.array[,,(num.valid.tables+1)] <- valid.output.array[,,(num.valid.tables+1)]+valid.output.array[,,j]
      }
      valid.output.array <- valid.output.array
      dimnames(valid.output.array) <- list(unique.categories.1,unique.categories.2,c(opals.names[opals.valid.id],"ALL VALID STUDIES COMBINED"))
      
      # object to return
      obj2return <- list(valid.output.array,num.cats.1,num.cats.2,unique.categories.1, unique.categories.2)
      names(obj2return) <- c("valid.output.array","num.cats.1","num.cats.2","unique.categories.1","unique.categories.2")
      
      return(obj2return)
  }  
} 
