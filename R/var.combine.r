#' Calculates the variance of a variable across several non-pooled datasets
#' @title Computes the variance of a variable 
#' @param opals character strings that represent the URL of the servers where 
#' the study datasets are stored
#' @param x a numeric vector, matrix or data frame
#' @details If xvect contains missing values, the latter are omitted (na.rm=TRUE).
#' If xvect is not present in any of the data sets, the variance of xvect is calculated 
#' only for those data sets that contain xvect. In addition, a warning message is produced.
#' @return a 
#' @references Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) 
#' The New S Language. Wadsworth & Brooks/Cole.
#' @author Isaeva, J.
#' @export
#' 
var.combine <- function(opals, x) {
  
  # Returns the variance across merged studies (na.rm=T since local var and mean are calculated with omitted NA's)
  # Using formula
  # var(x) = 1/(n-1)*(sum(x_i^2)-n*x_mean^2)
  
  num.sources = length(opals)
  
  cally = call('complete.cases', x)
  datashield.assign(opals, 'not.na.obs', cally)
  cally = call('subset', x, quote(not.na.obs))
  datashield.assign(opals, 'xok', cally)
  
  var.local = datashield.aggregate(opals, quote(var(xok)))
  mean.local = datashield.aggregate(opals, quote(colMeans(xok)))
  length.local = datashield.aggregate(opals, quote(NROW(xok)))
  
  length.total = 0
  mean.global = NA
  sum.weighted.x = 0
  
  for (i in 1:num.sources) {    # calculating global mean
    if (is.null(var.local[[i]])) {
      txt = c('Variable is not present in study ',i)
      warning(txt)
    }
    if (!is.null(mean.local[[i]])) {
      sum.weighted.x = sum.weighted.x + length.local[[i]]*mean.local[[i]]
      length.total = length.total+length.local[[i]]
    }
  }
  
  if (!is.na(sum.weighted.x))
    mean.global = sum.weighted.x/length.total else
      stop('Check the data supplied: global mean is NA')
  
  
  nrows_var = NROW(var.local[[1]])
  ncols_var = NCOL(var.local[[1]])
  dummy.sum = matrix(0, nrows_var, ncols_var)
  
  for (i in 1:num.sources) {
    if (!is.null(var.local[[i]]) & !is.null(mean.local[[i]]))
      if (!is.na(var.local[[i]]) & !is.na(mean.local[[i]])) {
        var.weight = (length.local[[i]]-1)*var.local[[i]]
        add.elem = length.local[[i]]*(mean.local[[i]]%x%t(mean.local[[i]]))
        dummy.sum = dummy.sum +var.weight+add.elem
      }      
  }
  mean.global.products = length.total*(mean.global%x%t(mean.global))
  var.global = 1/(length.total-1)*(dummy.sum-mean.global.products)
  
  datashield.rm(opals, 'not.na.obs')
  datashield.rm(opals, 'xok')
  
  cat('\n\nGlobal variance:\n\n')
  print(var.global)
  cat('\n\n')
  
}