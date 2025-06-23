#'
#' @title Generates subset tables
#' @description This is an internal function.
#' @details This function is called by the function 'ds.meanByClass' to break down
#' the initial table by the specified categorical variables.
#' @param dtsource a list of \code{\link[DSI]{DSConnection-class}} objects obtained after login. If the <datasources>
#' the default set of connections will be used: see \link[DSI]{datashield.connections_default}.
#' @param tables a character vector, the tables to breakdown
#' @param variable a character, the variable to subset on
#' @param categories a character vector, the classes in the variables to subset on
#' @keywords internal
#' @return a character the names of the new subset tables.
#' @author Gaye, A.
#'
meanByClassHelper1 <- function(dtsource, tables, variable, categories){

  newtablenames <- c()
  for(i in 1:length(tables)){
    dt2process <- tables[i]
    # look for invalid and empty tables
    check1 <- which(unlist(strsplit(dt2process,"_")) == "INVALID")
    check2 <- which(unlist(strsplit(dt2process,"_")) == "EMPTY")
    if(length(check1) > 0 | length(check2) > 0){
      newtablenames <- append(newtablenames, meanByClassHelper4(dtsource, 'tempholder', dt2process, variable, categories))
    }else{
      if(is.null(variable)){
        cally <- paste0("subsetByClassDS('", dt2process, "')")
      }else{
        cally <- paste0("subsetByClassDS('", dt2process, "', c('",paste(variable,collapse="','"),"'))")
      }
      DSI::datashield.assign(dtsource, 'tempholder', as.symbol(cally))
      newtablenames <- append(newtablenames, meanByClassHelper4(dtsource, 'tempholder', dt2process))
    }
  }

  return(newtablenames)
}
