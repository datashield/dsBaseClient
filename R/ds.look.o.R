#' 
#' @title ds.look.o 
#' @description  The function is a wrapper for the 'opal' package function 'datashield.aggregate'.
#' @details the function {ds.look.o} can be used to make a direct call to a server side'
#' aggregate function more simply than using the {datashield.aggregate} function.
#' The {ds.look.o} and {datashield.aggregate} functions are generally
#' only recommended for experienced developers. For example, the toAggregate argument has to
#' be expressed in the same form that the serverside function would usually expect from its
#' clientside pair. For example: ds.look.o("table1DDS(female)") works. But, if you express
#' this as ds.look.o("table1DDS('female')") it won't work because although when
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
#' @param datasources specifies the particular opal object(s) to use. If the <datasources>
#' argument is not specified the default set of opals will be used. The default opals
#' are called default.opals and the default can be set using the function
#' {ds.setDefaultOpals.o}. If the <datasources> is to be specified, it should be set without
#' inverted commas: e.g. datasources=opals.em or datasources=default.opals. If you wish to
#' apply the function solely to e.g. the second opal server in a set of three,
#' the argument can be specified as: e.g. datasources=opals.em[2].
#' If you wish to specify the first and third opal servers in a set you specify:
#' e.g. datasources=opals.em[c(1,3)]
#' @return the output from the specified server side aggregate function to the client side
#' @author Gaye, A, Burton PR.
#' @export
ds.look.o<-function(toAggregate=NULL, checks=FALSE, datasources=NULL){

#####################################################################################
#MODULE 1: IDENTIFY DEFAULT OPALS  													#
  # if no opal login details are provided look for 'opal' objects in the environment#
  if(is.null(datasources)){															#
    datasources <- findLoginObjects()												#
  }																					#						
#####################################################################################  

###########################################################################################
#MODULE 3: OPTIONAL CHECKS FOR KEY DATA OBJECTS                                           #
#IN DIFFERENT SOURCES                                                                     #
if(checks==TRUE){                                                                         #
  if(is.null(toAggregate)){                                                               #
    stop("<toAggregate> missing, please give an expression/function in inverted commas\n",#
	call.=FALSE)                                                                          #
  }                                                                                       #
}                                                                                         #
###########################################################################################


  # now do the business
  output<-opal::datashield.aggregate(datasources, as.symbol(toAggregate))
  return(list(output=output))
  }
  
#ds.look.o

