#' @title ds.unList.o calling aggregate function unListDS.o
#' @description this function is based on the native R function {unlist}
#' which coerces an object of list class back to the class it was when
#' it was coerced into a list
#' @details See details of the native R function {unlist}.
#' Unlike most other class coercing functions the serverside function
#' that is called is an aggregate function rather than an assign function.
#' This is because the {datashield.assign} function in opal deals
#' specially with a created object (<newobj>) if it is of class list.
#' Reconfiguring the function as an aggregate function
#' works around this problem. When an object is coerced to a list, depending
#' on the class of the original object some information may be lost. Thus,
#' for example, when a data.frame is coerced to a list information that
#' underpins the structure of the data.frame is lost and when it is
#' subject to the function {ds.unlist.o} it is returned to a simpler
#' class than data.frame eg 'numeric' (basically a numeric vector
#' containing all of the original data in all variables in the data.frame
#' but with no structure). If you wish to reconstruct the original
#' data.frame you therefore need to specify this structure again e.g.
#' the column names etc 
#' @param x.name the name of the input object to be unlisted.
#' It must be specified in inverted commas e.g. x.name="input.object.name"
#' @param recursive logical, if FALSE the function will not recurse
#' beyond the first level items in x (e.g. the N data sources in many
#' DataSHIELD settings. Default = TRUE so recursion includes all levels.
#' @param newobj the name of the new output variable. If this argument is set
#' to NULL, the name of the new variable is defaulted to <x.name>.unlist
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
#' @return the object specified by the <newobj> argument (or by default <x.name>.unlist
#' if the <newobj> argument is NULL) which is written to the serverside.
#' In addition, two validity messages are returned. The first confirms an output
#' object has been created, the second states its class. The way that {as.list}
#' coerces objects to list depends on the class of the object, and so
#' the class of the unlisted output will depend on how the original
#' list was formed - see details
#' @author Amadou Gaye, Paul Burton, for DataSHIELD Development Team
#' @export
ds.unList.o <- function(x.name=NULL, recursive=TRUE, newobj=NULL, datasources=NULL){
  
  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }
  
  if(is.null(x.name)){
    stop("Please provide the name of the input vector!", call.=FALSE)
  }
  
  
  # create a name by default if user did not provide a name for the new variable
  if(is.null(newobj)){
    newobj <- paste0(x.name, ".unlist")
  }

  recursive<-recursive
  
     # call the server side function that does the job

	calltext <- call("unListDS.o", x.name, recursive, newobj)

	out.message<-opal::datashield.aggregate(datasources, calltext)
#	print(out.message)
#Don't include assign function completion module as it can print out an unhelpful
#warning message when newobj is a list

}
# ds.unList.o
