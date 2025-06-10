#'
#' @title Gets a pooled variance
#' @description This is an internal function.
#' @details This function is called to avoid calling the client function 'ds.var'
#' which may stop the process due to some checks not required when computing a mean inside
#' a function.
#' @param dtsources a list of \code{\link[DSI]{DSConnection-class}} objects obtained after login. If the <datasources>
#' the default set of connections will be used: see \link[DSI]{datashield.connections_default}.
#' @param x a character, the name of a numeric vector
#' @keywords internal
#' @return a pooled variance
#'
getPooledVar <- function(dtsources, x){

  num.sources <- length(dtsources)

  cally <- paste0("varDS(", x, ")")
  out.var <- DSI::datashield.aggregate(dtsources, as.symbol(cally))

  length.total <- 0
  sum.weighted <- 0
  var.global <- NA

  for (i in 1:num.sources){
    if ((!is.null(out.var[[i]][[5]])) & (out.var[[i]][[5]]!=0)) {
      var.local <- out.var[[i]][[2]]/(out.var[[i]][[4]]-1) - (out.var[[i]][[1]])^2/(out.var[[i]][[4]]*(out.var[[i]][[4]]-1))
      completeLength <- out.var[[i]][[5]]-out.var[[i]][[3]]
      length.total = length.total+completeLength
      sum.weighted = sum.weighted+completeLength*var.local
    }
  }

  var.global = sum.weighted/length.total
  return(var.global)

}
