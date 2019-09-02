#' @title ds.asList calling aggregate function asListDS
#' @description this function is based on the native R function {as.list}
#' @details See details of the native R function {as.list}.
#' Unlike most other class coercing functions the serverside function
#' that is called is an aggregate function rather than an assign function.
#' This is because the {datashield.assign} function in DSI deals
#' specially with a created object (<newobj>) if it is of class list.
#' Reconfiguring the function as an aggregate function
#' works around this problem.
#' @param x.name the name of the input object to be coerced to class
#' list. Must be specified in inverted commas e.g. x.name="input.object.name"
#' @param newobj the name of the new output variable. If this argument is set
#' to NULL, the name of the new variable is defaulted to <x.name>.list
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login. If the <datasources>
#' the default set of connections will be used: see \link{datashield.connections_default}.
#' @return the object specified by the <newobj> argument (or by default <x.name>.list
#' if the <newobj> argument is NULL) which is written to the serverside.
#' In addition, two validity messages are returned. The first confirms an output
#' object has been created, the second states its class. The way that {as.list}
#' coerces objects to list depends on the class of the object, but in general
#' the class of the output object should usually be 'list'
#' @author Amadou Gaye, Paul Burton, for DataSHIELD Development Team
#' @export
ds.asList = function(x.name=NULL, newobj=NULL, datasources=NULL){

  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  if(is.null(x.name)){
    stop("Please provide the name of the input vector!", call.=FALSE)
  }


  # create a name by default if user did not provide a name for the new variable
  if(is.null(newobj)){
    newobj <- paste0(x.name, ".list")
  }

  # call the server side function that does the job

  calltext <- call("asListDS", x.name, newobj)

  out.message<-DSI::datashield.aggregate(datasources, calltext)
# print(out.message)

#Don't include assign function completion module as it can print out an unhelpful
#warning message when newobj is a list

}
# ds.asList
