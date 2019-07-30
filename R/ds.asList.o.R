#' @title ds.asList.o calling aggregate function asListDS.o
#' @description this function is based on the native R function {as.list}
#' @details See details of the native R function {as.list}.
#' Unlike most other class coercing functions the serverside function
#' that is called is an aggregate function rather than an assign function.
#' This is because the {datashield.assign} function in opal deals
#' specially with a created object (<newobj>) if it is of class list.
#' Reconfiguring the function as an aggregate function
#' works around this problem.
#' @param x.name the name of the input object to be coerced to class
#' list. Must be specified in inverted commas e.g. x.name="input.object.name"
#' @param newobj the name of the new output variable. If this argument is set
#' to NULL, the name of the new variable is defaulted to <x.name>.list
#' @param datasources specifies the particular opal object(s) to use. If the <datasources>
#' argument is not specified the default set of opals will be used. The default opals
#' are called default.opals and the default can be set using the function
#' {ds.setDefaultOpals.o}. If an explicit <datasources> argument is to be set,
#' it should be specified without
#' inverted commas: e.g. datasources=opals.em or datasources=default.opals. If you wish to
#' apply the function solely to e.g. the second opal server in a set of three,
#' the argument can be specified as: e.g. datasources=opals.em[2].
#' If you wish to specify the first and third opal servers in a set you specify:
#' e.g. datasources=opals.em[c(1,3)]
#' @return the object specified by the <newobj> argument (or by default <x.name>.list
#' if the <newobj> argument is NULL) which is written to the serverside.
#' In addition, two validity messages are returned. The first confirms an output
#' object has been created, the second states its class. The way that {as.list}
#' coerces objects to list depends on the class of the object, but in general
#' the class of the output object should usually be 'list'
#' @author Amadou Gaye, Paul Burton, for DataSHIELD Development Team
#' @export
ds.asList.o = function(x.name=NULL, newobj=NULL, datasources=NULL){
  
  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }
  
  if(is.null(x.name)){
    stop("Please provide the name of the input vector!", call.=FALSE)
  }
  
  
  # create a name by default if user did not provide a name for the new variable
  if(is.null(newobj)){
    newobj <- paste0(x.name, ".list")
  }

  # call the server side function that does the job

  calltext <- call("asListDS.o", x.name, newobj)

  out.message<-opal::datashield.aggregate(datasources, calltext)
# print(out.message)

#Don't include assign function completion module as it can print out an unhelpful
#warning message when newobj is a list

}
# ds.asList.o
