#' @title ds.listServersideFunctions.o calling no server-side functions
#' @description Lists all current server-side functions
#' @details Uses dsadmin.get_methods function from opaladmin package to list all
#' assign and aggregate functions on the available opal servers. The only choice of
#' arguments is in datasources; i.e. which studies to interrogate. Once the studies have 
#' been selected ds.listServersideFunctions.o lists all assign functions for all 
#' of these studies and then all aggregate functions for all of them.
#' @param datasources specifies the particular opal object(s) to use. If the <datasources>
#' argument is not specified the default set of opals will be used. The default opals
#' are called default.opals and the default can be set using the function
#' {ds.setDefaultOpals.o}. If the <datasources> is to be specified, it should be set without
#' inverted commas: e.g. datasources=opals.em or datasources=default.opals. If you wish to
#' apply the function solely to e.g. the second opal server in a set of three,
#' the argument can be specified as: e.g. datasources=opals.em[2].
#' If you wish to specify the first and third opal servers in a set you specify:
#' e.g. datasources=opals.em[c(1,3)]
#' @return list containing all serverside functions by study. Firstly lists assign
#' and then aggregate functions.
#' @author Burton, PR.
#' @export
ds.listServersideFunctions.o<-function(datasources=NULL){



	# if no opal login details are provided look for 'opal' objects in the environment
	if(is.null(datasources)){
	    datasources <- findLoginObjects()
	}

	num.datasources<-length(datasources)

	assign.funs<-opaladmin::dsadmin.get_methods(datasources, 'assign')
	aggregate.funs<-opaladmin::dsadmin.get_methods(datasources, 'aggregate')
	
	serverside.assign.functions<-list()
	serverside.aggregate.functions<-list()

	for(j in 1:num.datasources){
		serverside.assign.functions[j]<-assign.funs[[j]][1]
		serverside.aggregate.functions[j]<-aggregate.funs[[j]][1]
	}

	names(serverside.assign.functions)=names(datasources)
	names(serverside.aggregate.functions)=names(datasources)
		
	return(list(serverside.assign.functions=serverside.assign.functions,
			serverside.aggregate.functions=serverside.aggregate.functions))
}

#ds.listServersideFunctions.o
