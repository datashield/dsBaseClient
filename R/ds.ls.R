#ds.ls
#' @title lists all objects on a serverside environment
#' @description creates a list of the names of all of the objects in
#' a specified serverside environment
#' @details Clientside function {ds.ls} calls serverside aggregate function
#' {lsDS}. When running analyses one may want to know the objects already generated. This 
#' request is not disclosive as it only returns the names of the objects and not their contents. 
#' By default, objects in DataSHIELD's 'active serverside analytic environment' (".GlobalEnv")
#' will be listed. This
#' is the environment that contains all of the objects that serverside DataSHIELD 
#' is using for the main analysis or has written out to the serverside during the process
#' of managing or undertaking the analysis (variables, scalars, matrices, data.frames etc). 
#' The environment to explore is specified by the argument env.to.search (i.e. environment
#' to search) to an integer value. The default environment 
#' which R names as ".GlobalEnv" is set by specifying env.to.search = 1 or 1L
#' (1L is just an explicit way of writing the integer 1). If the Boolean argument search.GlobalEnv
#' is set to TRUE env.to.search is set to 1L regardless what value it is set in the call
#' or if it is set to NULL. So if search.GlobalEnv is set to TRUE, ds.ls will automatically
#' search the ".GlobalEnv" R environment on the serverside which contains all of the
#' variables, data.frames and other objects read in at the start of the analysis,
#' as well as any new objects of any sort 
#' created using DataSHIELD assign functions. Other serverside environments contain other
#' objects. For example, environment 2L contains the functions loaded via the native R
#' stats package and 6L contains the standard list of datasets built into R. By default
#' {ds.ls} will return a list of ALL of the objects in the environment specified by the
#' env.to.search argument but you can specify search filters including '*' wildcards
#' using the search.filter argument
#' @param search.filter either NULL or a character string (potentially including '*'
#' wildcards). Thus, search.filter="Sd2*" will list the names of all objects in the specified
#' environment with names beginning capital S, lower case d and number 2. Similarly,
#' search.filter="*.ID" will return all objects with names ending with ".ID" eg "Study.ID".
#' If a value is not specified for the search.filter argument, or it is set as NULL, the names of 
#' all objects in the specified environment will be returned.
#' @param env.to.search integer (e.g. in '2' or '2L' format) specifying the position
#' in the search path of the environment to be explored. 1L is current active analytic
#' environment on serverside and is the default value of env.to.search. Other environments
#' can be explored instead by explicitly specifying the value of env.to.search in the
#' clientside call (see details [above])
#' @param search.GlobalEnv a Boolean function. If TRUE, {ds.ls} will list all objects
#' in the .GlobalEnv R environment on the serverside. If FALSE and if env.to.search is also
#' set as a valid integer, {ds.ls} will list all objects in the serverside R environment
#' identified by pos=env.to.search in the search path. 
#' @param datasources specifies the particular 'connection object(s)' to use.
#' e.g. if you have several data sets in the sources you are working with
#' called opals.a, opals.w2, and connection.xyz, you can choose which of
#' these to work with. The call 'datashield.connections_find()' lists all of
#' the different datasets available and if one of these is called 'default.connections'
#' that will be the dataset used by default if no other dataset is specified. If you
#' wish to change the connections you wish to use by default the call
#' datashield.connections_default('opals.a') will set 'default.connections'
#' to be 'opals.a' and so in the absence of specific instructions to the contrary
#' (e.g. by specifiying a particular dataset to be used via the <datasources>
#' argument) all subsequent function calls will be to the datasets held in opals.a.
#' If the <datasources> argument is specified, it should be set without
#' inverted commas: e.g. datasources=opals.a or datasources=default.connections.
#' The <datasources> argument also allows you to apply a function solely to a subset
#' of the studies/sources you are working with. For example, the second source
#' in a set of three, can be specified using a call such as datasources=connection.xyz[2].
#' On the other hand, if you wish to specify solely the first and third sources, the
#' appropriate call will be datasources=connections.xyz[c(1,3)]
#' @return a list containing: (1) the name/details of the serverside R environment
#' which {ds.ls} has searched; (2) a vector of character strings giving the names of
#' all objects meeting the naming criteria specified by the argument <search.filter> in this
#' specified R serverside environment; (3) the nature of the search filter string as it was
#' actually applied
#' @author Gaye, A (2015). Updated and extended by Paul Burton (2020).
#' @export
ds.ls <- function(search.filter=NULL, env.to.search=1L, search.GlobalEnv=TRUE, datasources=NULL){
  
   if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

#make default to .GlobalEnv unambiguous

if(search.GlobalEnv||is.null(env.to.search))
{
env.to.search<-1L
}

	 

#make code compatible with ds.passParser
transmit.object<-search.filter
transmit.object.temp1<-NULL

#set up character replacement
input.string<-"*"
replacement.string<-"_:A:_"
replacement.string.split<-unlist(strsplit(replacement.string,split=""))
length.rs<-length(replacement.string.split)
 
#Search for *s in code and convert to transmittable code
if(!is.null(transmit.object))
{
	transmit.object.split<-unlist(strsplit(transmit.object,split=""))

	length.to<-length(transmit.object.split)

	#first check that replacement character string does not appear in original search.filter code 

	if(length.to>=length.rs)
	{
		for(k in 1:(length.to-length.rs+1))
		{
			original.code.problem<-TRUE
			for(m in 1:length.rs)
			{
			if(transmit.object.split[k+m-1]!=replacement.string.split[m])
				{
				original.code.problem<-FALSE
				}
			if(original.code.problem==TRUE)
				{
	#			return.message<-paste0("Warning: Code replacing wildcard (i.e. '",replacement string,"' appears in your original code -please respecify")
				return.message<-paste0("Warning: Code replacing wildcard (i.e. '",input.string,
				"') is '",replacement.string,"' but this appears in your original search filter string - please respecify")
				return(return.message)
				}
			}
		}
	}

	for(j in 1:length.to)
	{
	add.to<-transmit.object.split[j]
		if(add.to=="*")
		{
		add.to<-"_:A:_"
		}
	transmit.object.temp1<-c(transmit.object.temp1,add.to)
	}
	transmit.object.final<-paste(transmit.object.temp1,collapse="")

	}else{
	transmit.object.final<-NULL
	}


  # call the server side function
  calltext <- call("lsDS",search.filter=transmit.object.final, env.to.search)

  output <- datashield.aggregate(datasources, calltext)
  
  return(output)
  }
#ds.ls


