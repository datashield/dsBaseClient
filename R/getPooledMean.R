#'
#' @title Gets a pooled statistical mean
#' @description This is an internal function.
#' @details This function is called to avoid calling the client function 'ds.mean'
#' which may stop the process due to some checks not required when computing a mean inside
#' a function.
#' @param dtsources a list of \code{\link[DSI]{DSConnection-class}} objects obtained after login. If the <datasources>
#' the default set of connections will be used: see \link[DSI]{datashield.connections_default}.
#' @param x a character, the name of a numeric vector
#' @keywords internal
#' @return a pooled mean
#'
getPooledMean <- function(dtsources, x){

  num.sources <- length(dtsources)

  cally <- paste0("meanDS(", x, ")")
  out.mean <- DSI::datashield.aggregate(dtsources, as.symbol(cally))

  length.total <- 0
  sum.weighted <- 0
  mean.global <- NA

  for (i in 1:num.sources){
    if ((!is.null(out.mean[[i]][[4]])) & (out.mean[[i]][[4]]!=0)) {
      completeLength <- out.mean[[i]][[4]]-out.mean[[i]][[2]]
      length.total = length.total+completeLength
      sum.weighted = sum.weighted+completeLength*out.mean[[i]][[1]]
    }
  }

  mean.global = sum.weighted/length.total
  return(mean.global)

}
