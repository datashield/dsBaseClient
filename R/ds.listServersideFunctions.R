#' @title ds.listServersideFunctions calling no server-side functions
#' @description Lists all current server-side functions
#' @details Uses \link{datashield.methods} function from DSI package to list all
#' assign and aggregate functions on the available data repository servers. The only choice of
#' arguments is in datasources; i.e. which studies to interrogate. Once the studies have
#' been selected ds.listServersideFunctions lists all assign functions for all
#' of these studies and then all aggregate functions for all of them.
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login. If the <datasources>
#' the default set of connections will be used: see \link{datashield.connections_default}.
#' @return list containing all serverside functions by study. Firstly lists assign
#' and then aggregate functions.
#' @author Burton, PR.
#' @export
#' @import DSI
ds.listServersideFunctions<-function(datasources=NULL){
	.Deprecated("DSI::datashield.methods")

  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  assign.funs <- DSI::datashield.methods(datasources, 'assign')
  aggregate.funs <- DSI::datashield.methods(datasources, 'aggregate')
  return(list(serverside.assign.functions=assign.funs,
              serverside.aggregate.functions=aggregate.funs))
}

#ds.listServersideFunctions
