#' @title ds.tapply.o calling tapplyDS.o
#' @description Apply one of a selected range of functions to summarize an
#' outcome variable over one or more indexing factors and write the resultant
#' summary to the clientside
#' @details A clientside function calling an aggregate serverside function that uses
#' the native R function tapply() to apply one of
#' a selected range of functions to each cell of a ragged array, that is to each (non-empty)
#' group of values given by each unique combination of a series of indexing factors. The native R
#' {tapply} function is very flexible and the range of allowable summarizing functions
#' is much more restrictive for the DataSHIELD ds.tapply.o function. This is to protect
#' against disclosure risk. At present the allowable functions are: N/length (the number
#' of (non-missing) observations in the group defined by each combination of indexing
#' factors; mean; SD (standard deviation); sum; quantile (with quantile probabilities set at
#' c(0.05,0.1,0.2,0.25,0.3,0.33,0.4,0.5,0.6,0.67,0.7,0.75,0.8,0.9,0.95). Should other functions
#' be required in the future then, provided they are non-disclosive, the DataSHIELD development
#' team could work on them if requested. As an aggregate function {ds.tapply.o} returns the
#' summarized values to the clientside. In order to protect against disclosure the 
#' number of observations in each summarizing group in each source is calculated
#' and if any of these fall below the value of nfilter.tab (the minimum allowable non-zero count
#' in a contingency table) the tapply analysis of that source will return only an error message.
#' The value of nfilter.tab is can be set and modified only by the data custodian. If an
#' analytic team wish the value to be reduced (e.g. to 1 which will allow any output
#' from tapply.o to be returned) this needs to formally be discussed and agreed
#' with the data custodian. If the reason for the tapply analysis is, for example, to break
#' a dataset down into a small number of values for each individual and then to flag up
#' which individuals have got at least one positive value for a binary outcome variable, then
#' that flagging does not have to be overtly returned to the clientside. Rather, it can be
#' written as a vector to the serverside at each source (which, like any other serverside
#' object, cannot then be seen, abstracted or copied). This can be done using ds.tapply.assign.o
#' which calls an assign function {tapplyDS.assign.o} which acts just like the aggregate
#' function {tapplyDS.o} but it writes the results as a newobj to the serverside and does
#' not test the number of observations in each group against nfilter.tab. The native R
#' tapply function has optional arguments such as na.rm=TRUE for FUN = mean which will
#' exclude any NAs from the outcome variable to be summarized. However, in order to keep
#' DataSHIELD's {ds.tapply.o} and {ds.tapply.assign.o} functions straightforward, the
#' serverside functions {tapplyDS.o} and {tapplyDS.assign.o} both start by stripping
#' any observations which have missing (NA) values in either the outcome variable or in
#' any one of the indexing factors. In consequence, the resultant analyses are always based
#' on complete.cases.   
#' @param X.name, the name of the variable to be summarized. The user must set the name as a
#' character string in inverted commas. For example: X.name="var.name"
#' @param INDEX.names, the name of a single factor or a vector of names of factors to
#' index the variable to be summarized. Each name must be specified in inverted commas.
#' For example: INDEX.names="factor.name" or
#' INDEX.names=c("factor1.name", "factor2.name", "factor3.name"). The native R tapply function
#' can coerce non-factor vectors into factors. However, this does not always work when
#' using the DataSHIELD ds.tapply.o/ds.tapply.assign.o functions so if you are concerned that
#' an indexing vector is not being treated correctly as a factor,
#' please first declare it explicitly as a factor using {ds.asFactor.o}   
#' @param FUN.name, the name of one of the allowable summarizing functions to be applied
#' specified in inverted commas. The present version of the
#' function allows the user to choose one of five summarizing functions. These are
#' "N" (or "length"), "mean","sd", "sum", or "quantile". For more information see Details.
#' @param datasources specifies the particular opal object(s) to use. If the <datasources>
#' argument is not specified the default set of opals will be used. The default opals
#' are called default.opals and the default can be set using the function
#' {ds.setDefaultOpals.o}. If the <datasources> is to be specified, it should be set without
#' inverted commas: e.g. datasources=opals.em or datasources=default.opals. If you wish to
#' apply the function solely to e.g. the second opal server in a set of three,
#' the argument can be specified as: e.g. datasources=opals.em[2].
#' If you wish to specify the first and third opal servers in a set you specify:
#' e.g. datasources=opals.em[c(1,3)]
#' @return an array of the summarized values created by the tapplyDS.o function. This array
#' is returned to the clientside. It has the same number of dimensions as INDEX.
#' @author Paul Burton, Demetris Avraam for DataSHIELD Development Team
#' @export
ds.tapply.o <- function(X.name=NULL, INDEX.names=NULL, FUN.name=NULL, datasources=NULL){

  ###datasources
  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }
  
  ###X.name
  # check if user has provided the name of the column that holds X.name
  if(is.null(X.name)){
    return("Error: Please provide the name of the variable to be summarized, as a character string")
  }

  ###INDEX.names
  # check if user has provided the name of the column(s) that holds INDEX.names
  if(is.null(INDEX.names)){
    Err.1 <- "Error: Please provide the name of the single factor or"
    Err.2 <- "the list of factors to index the variable to be summarized."
    Err.3 <- "In either case the argument must be specified in inverted commas"
    return(list(Error.message=Err.1, Err.cont2=Err.2, Err.cont3=Err.3))
  }

 #make INDEX.names transmitable
  if(!is.null(INDEX.names)){
    INDEX.names.transmit <- paste(INDEX.names,collapse=",")
  }else{
    INDEX.names.transmit <- NULL
  }

  ###FUN.name  
  # check if user has provided a valid summarizing function
  if(is.null(FUN.name)){
    return("Error: Please provide a valid summarizing function, as a character string")
  }

  # CALL THE PRIMARY SERVER SIDE FUNCTION
  calltext <- call("tapplyDS.o", X.name, INDEX.names.transmit, FUN.name)
 
  output <- opal::datashield.aggregate(datasources, calltext)
 
  return(output)
  
}
#ds.tapply.o

