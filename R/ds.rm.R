#' @title ds.rm calling aggregate function rmDS
#' @description deletes an R object on the serverside
#' @details this clientside function calls a serverside function
#' based on the rm() function in native R. This is the
#' aggregate function rmDS. The fact that it is an aggregate
#' function maybe surprising because it modifies an object
#' on the serverside, and would therefore be expected to be an assign function.
#' However, as an assign function the last step in running it
#' would be to write the modified object as newobj. But this would
#' fail because the effect of the function is to delete the object and so
#' it would be impossible to write it anywhere. Please note that although
#' this calls an aggregate function there is no <type> argument.
#' @param x.name, the name of the object to be deleted specified in
#' inverted commas. For example: x.name='object.name'.
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login. If the <datasources>
#' the default set of connections will be used: see \link{datashield.connections_default}.
#' @return the specified object is deleted from the serverside. If this
#' is successful the message "Object <x.name> successfully deleted" is returned
#' to the clientside (where x.name is the name of the object to be deleted).
#' If the object to be deleted is already absent on a given
#' source, that source will return the message: "Object to be deleted, i.e. <x.name> ,
#' does not exist so does not need deleting". Finally, if the specified name
#' of the object to be deleted is too long (>nfilter.stringShort) there is
#' a potential disclosure risk (active code hidden in the name) and the
#' serverside function returns a message such as: "Disclosure risk, number of characters
#' in x.name must not exceed nfilter.stringShort which is currently set at: 25" where
#' '25' is the current setting of the R_Option value of nfilter.stringShort.
#' @author Paul Burton for DataSHIELD Development Team
#' @export
ds.rm<-function(x.name=NULL, datasources=NULL){

  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  if(is.null(x.name)){
   stop("Please provide the name of the object to be deleted (eg 'object.name') as the x.name argument", call.=FALSE)
  }

	#make transmittable via parser
    x.name.transmit <- paste(x.name,collapse=",")


  # call the server side function
  #PLEASE NOTE THIS IS - SURPRISINGLY - AN AGGREGATE FUNCTION: see details in header

	calltext <- call("rmDS", x.name.transmit)

	output = DSI::datashield.aggregate(datasources, calltext)

  return(output)
}

#ds.rm
