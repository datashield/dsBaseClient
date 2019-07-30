#' @title ds.setSeed.o calling setSeedDS.o
#' @description Primes the pseudorandom number generator in a data source 
#' @details ds.setSeed.o calls the aggregate function setSeedDS.o in each
#' data source, passing it a single integer value 
#' which acts as a trigger value in that source to generate an instance of
#' the full pseudorandom number seed that is a vector of integers
#' of length 626 called .Random.seed. Each time a new
#' pseudorandom number is generated, the current .Random.seed vector
#' provides a deterministic but very close to behaviourally random way to generate
#' that pseudorandom number and to completely regenerate the random seed vector.
#' Unusually, because setSeedDS.o is effectively the same as the set.seed() function
#' in native R, although it writes a new object to the serverside (i.e. the integer
#' vector of length 626 known as .Random.seed [see info for the
#' argument <seed.as.integer>]) because this is done directly via calling a native
#' R function this has been set up as an aggregate function not an assign function.
#' @param seed.as.integer a numeric scalar or a NULL which primes the random seed
#' in each data source. The current limitation on the value of the integer that
#' can be specified is -2147483647 up to +2147483647 (this is +/- ([2^31]-1)).
#' Because you only specify one integer in the call to ds.setSeed.o 
#' (i.e. the value for the <seed.as.integer> argument) that value will be 
#' used as the priming trigger value in all of the specified
#' data sources and so the pseudorandom number generators will all start from
#' the same position and if a vector of pseudorandom number values is requested
#' based on one of DataSHIELD's pseudorandom number generating functions precisely
#' the same random vector will be generated in each source. If you want to avoid this
#' you can specify a different priming value in each source by using the
#' the <datasources> argument to generate the random number vectors one source
#' at a time with a different integer in each case. Furthermore, if you use any one
#' of DataSHIELD's pseudorandom number generating functions (ds.rNorm.o, ds.rUnif.o,
#' ds.rPois.o or ds.rBinom.o) the function call itself automatically uses the single
#' integer priming seed you specify to generate different integers in each source.
#' Thus, by default, when you are generating pseudorandom number vectors in a series
#' of different data sources using the standard DataSHIELD functions the vectors
#' will be different in each source. Given the inbuilt choice of arguments
#' for set.seed() that are fixed in DataSHIELD's setSeedDS.o function, if a given
#' priming integer is specified as the argument <seed.as.integer> in ds.setSeed.o
#' the .Random.seed vector that will be generated will be the same on any
#' data source internationally (regardless of the flavour of R). Please note
#' that the first two elements of .Random.seed do not vary meaningfully,
#' in particular, before a seed is set, element 2 varies between different R
#' platforms, but once the seed has been set, it becomes 624 which happens then
#' to be the length of the remaining 624 elements (3-626) of .Random.seed which
#' provide the meaningful component of the random number seed.
#' @param datasources specifies the particular opal object(s) to use. If the <datasources>
#' argument is not specified the default set of opals will be used. The default opals
#' are called default.opals and the default can be set using the function
#' {ds.setDefaultOpals.o}. If the <datasources> is to be specified, it should be set without
#' inverted commas: e.g. datasources=opals.em or datasources=default.opals. If you wish to
#' apply the function solely to e.g. the second opal server in a set of three,
#' the argument can be specified as: e.g. datasources=opals.em[2].
#' If you wish to specify the first and third opal servers in a set you specify:
#' e.g. datasources=opals.em[c(1,3)]
#' @return Sets the values of the vector of integers of length 626 known as
#' .Random.seed on each data source that is the true current state of the
#' random seed in each source. Also returns the value of the trigger
#' integer that has primed the random seed vector (.Random.seed) in
#' each source and also the integer vector (626 elements)
#' that is .Random.seed itself.  
#' @author Paul Burton for DataSHIELD Development Team
#' @export
ds.setSeed.o<-function(seed.as.integer=NULL,datasources=NULL){

##################################################################################
# if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }


seed.valid<-0

if(is.null(seed.as.integer)){
seed.as.text<-"NULL"
seed.valid<-1
}

if(is.numeric(seed.as.integer)){
seed.as.text<-as.character(seed.as.integer)
seed.valid<-1
}


if(seed.valid==0){
mess1<-("ERROR terminated: seed.as.integer must be set as an integer [numeric] or as being NULL")
return(mess1)
}

  calltext <- paste0("setSeedDS.o(", seed.as.text, ")")
  ssDS.obj <- opal::datashield.aggregate(datasources, as.symbol(calltext))

  return.message<-paste0("Trigger integer to prime random seed = ",seed.as.text)
  
  return(list(status.message=return.message,seed.as.set=ssDS.obj))
}
# ds.setSeed.o

