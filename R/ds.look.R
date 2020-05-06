#'
#' @title ds.look
#' @description  The function is a wrapper for the DSI package function 'datashield.aggregate'.
#' @details the function {ds.look} can be used to make a direct call to a server side'
#' aggregate function more simply than using the {datashield.aggregate} function.
#' The {ds.look} and {datashield.aggregate} functions are generally
#' only recommended for experienced developers. For example, the toAggregate argument has to
#' be expressed in the same form that the serverside function would usually expect from its
#' clientside pair. For example: ds.look("table1DDS(female)") works. But, if you express
#' this as ds.look("table1DDS('female')") it won't work because although when
#' you call this same function using its clientside function you write ds.table1D('female')
#' the inverted commas are stripped off during processing by the clientside function so the
#' call to the serverside does not contain inverted commas. Apart from during development
#' work (e.g. before a clientside function has been written) it is almost always easier
#' and less error prone to call a serverside function using its client-side pair.
#' @param toAggregate a character string specifying the function call to be made(see above)
#' @param checks a Boolean object indicating whether optional checks are to be undertaken
#' in this instance only one check is specified - to check that a "toAggregate" expression
#' has been specified, nevertheless this is defaulted to FALSE to be consistent with other
#' functions and to save a small amount of time.
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login. If the <datasources>
#' the default set of connections will be used: see \link{datashield.connections_default}.
#' @return the output from the specified server side aggregate function to the client side
#' @author Gaye, A, Burton PR.
#' @export
ds.look<-function(toAggregate=NULL, checks=FALSE, datasources=NULL){

#####################################################################################
#MODULE 1: IDENTIFY DEFAULT DS CONNECTIONS  													              #
  # look for DS connections                                                         #
  if(is.null(datasources)){															                            #
    datasources <- datashield.connections_find()												            #
  }																					                                        #
#####################################################################################

###########################################################################################
#MODULE 3: OPTIONAL CHECKS FOR KEY DATA OBJECTS                                           #
#IN DIFFERENT SOURCES                                                                     #
if(checks==TRUE){                                                                         #
  if(is.null(toAggregate)){                                                               #
    stop("<toAggregate> missing, please give an expression/function in inverted commas\n",#
	call.=FALSE)                                                                            #
  }                                                                                       #
}                                                                                         #
###########################################################################################


  # now do the business
  output<-DSI::datashield.aggregate(datasources, as.symbol(toAggregate))
  return(list(output=output))
  }

#ds.look
