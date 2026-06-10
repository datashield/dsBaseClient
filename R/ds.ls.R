#' @title lists all objects on a server-side environment
#' @description creates a list of the names of all of the objects in
#' a specified serverside environment. 
#' @details When running analyses one may want to know the objects already generated. This 
#' request is not disclosive as it only returns the names of the objects and not their contents. 
#' 
#' By default, objects in DataSHIELD's Active Serverside Analytic Environment (\code{.GlobalEnv})
#' will be listed. This is the environment that contains all of the objects that server-side DataSHIELD 
#' is using for the main analysis or has written out to the server-side during the process
#' of managing or undertaking the analysis (variables, scalars, matrices, data frames, etc).
#'  
#' The environment to explore is specified by the argument \code{env.to.search} (i.e. environment
#' to search) to an integer value. The default environment 
#' which R names as \code{.GlobalEnv} is set by specifying \code{env.to.search = 1} or \code{1L}
#' (\code{1L} is just an explicit way of writing the integer \code{1}). 
#' 
#' If the \code{search.GlobalEnv} argument is set to TRUE the \code{env.to.search} parameter  
#' is set to \code{1L} regardless of what value it is set in the call
#' or if it is set to NULL. 
#' So, if \code{search.GlobalEnv} is set to TRUE, \code{ds.ls} will automatically
#' search the \code{.GlobalEnv} R environment on the server-side which contains all of the
#' variables, data frames and other objects read in at the start of the analysis,
#' as well as any new objects of any sort created using DataSHIELD assign functions. 
#' 
#' Other server-side environments contain other
#' objects. For example, environment \code{2L} contains the functions loaded via the native R
#' stats package and \code{6L} contains the standard list of datasets built into R. By default
#' \code{ds.ls} will return a list of ALL of the objects in the environment specified by the
#' \code{env.to.search} argument but you can specify search filters including \code{*} wildcards
#' using the \code{search.filter} argument.
#' 
#' In \code{search.filter} you can use the symbol \code{*} to find all the object that contains
#' the specified characters. For example, \code{search.filter = "Sd2*"} 
#' will list the names of all objects in the specified
#' environment with names beginning capital S, lower case d and number 2. 
#' Similarly, \code{search.filter="*.ID"} will return all objects with names ending with \code{.ID},
#' for example \code{Study.ID}. 
#' If a value is not specified for the \code{search.filter} argument or it is set as NULL, the names of 
#' all objects in the specified environment will be returned.
#' 
#' Server function called: \code{lsDS}. 
#' 
#' @param search.filter character string (potentially including \code{*} symbol) specifying the filter 
#' for the object name that you want to find in the environment. For more information see \strong{Details}. 
#' @param env.to.search an integer (e.g. in \code{2} or \code{2L} format) specifying the position
#' in the search path of the environment to be explored. \code{1L} is the current active analytic
#' environment on the server-side and is the default value of \code{env.to.search}.
#' For more information see \strong{Details}.
#' @param search.GlobalEnv Logical. If TRUE, \code{ds.ls} will list all objects
#' in the \code{.GlobalEnv} R environment on the server-side. If FALSE and if \code{env.to.search} is also
#' set as a valid integer, \code{ds.ls} will list all objects in the server-side R environment
#' identified by \code{env.to.search} in the search path. 
#' For more information see \strong{Details}.
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} 
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#' @return \code{ds.ls} returns to the client-side a list containing: \cr
#' (1) the name/details of the server-side R environment which \code{ds.ls} has searched;\cr
#' (2) a vector of character strings giving the names of
#' all objects meeting the naming criteria specified by the argument \code{search.filter} in this
#' specified R server-side environment;\cr
#' (3) the nature of the search filter string as it was applied. 
#' @author DataSHIELD Development Team
#' @examples
#' \dontrun{
#'
#'   ## Version 6, for version 5 see the Wiki
#'   
#'   # connecting to the Opal servers
#' 
#'   require('DSI')
#'   require('DSOpal')
#'   require('dsBaseClient')
#'
#'   builder <- DSI::newDSLoginBuilder()
#'   builder$append(server = "study1", 
#'                  url = "http://192.168.56.100:8080/", 
#'                  user = "administrator", password = "datashield_test&", 
#'                  table = "CNSIM.CNSIM1", driver = "OpalDriver")
#'   builder$append(server = "study2", 
#'                  url = "http://192.168.56.100:8080/", 
#'                  user = "administrator", password = "datashield_test&", 
#'                  table = "CNSIM.CNSIM2", driver = "OpalDriver")
#'   builder$append(server = "study3",
#'                  url = "http://192.168.56.100:8080/", 
#'                  user = "administrator", password = "datashield_test&", 
#'                  table = "CNSIM.CNSIM3", driver = "OpalDriver")
#'   logindata <- builder$build()
#'   
#'   connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D") 
#'   
#'   #Example 1: Obtain the list of  all objects on a server-side environment
#'   
#'   ds.ls(datasources = connections)
#'   
#'   #Example 2: Obtain the list of all objects that contain "var" character in the name
#'   #Create in the server-side variables with "var" character in the name
#'   
#'   ds.assign(toAssign = "D$LAB_TSC",
#'             newobj = "var.LAB_TSC",
#'             datasources = connections)
#'   ds.assign(toAssign = "D$LAB_TRIG",
#'             newobj = "var.LAB_TRIG",
#'             datasources = connections)
#'   ds.assign(toAssign = "D$LAB_HDL",
#'             newobj = "var.LAB_HDL",
#'             datasources = connections)
#'   
#'   ds.ls(search.filter = "var*",
#'         env.to.search = 1L,
#'         search.GlobalEnv = TRUE,
#'         datasources = connections)
#'   
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(connections)
#' }
#'
#' @export
ds.ls <- function(search.filter=NULL, env.to.search=1L, search.GlobalEnv=TRUE, datasources=NULL){
  
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }

  # make default to .GlobalEnv unambiguous
  if(search.GlobalEnv||is.null(env.to.search)){
    env.to.search<-1L
  }

  # make code compatible with ds.passParser
  transmit.object <- search.filter
  transmit.object.temp1 <- NULL

  # set up character replacement
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
  calltext <- call("lsDS", search.filter=transmit.object.final, env.to.search)
  output <- datashield.aggregate(datasources, calltext)
  
  return(output)
  
}
#ds.ls
