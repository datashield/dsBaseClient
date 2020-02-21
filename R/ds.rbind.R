#' @title Combines R objects by rows to generate a matrix in the Opal servers 
#' @description Take a sequence of vector, matrix or data-frame arguments
#' and combine them by rows to produce a matrix.
#' @details A sequence of vector, matrix or data-frame arguments
#' is combined  by rows to produce a matrix in the Opal servers.
#' 
#' In <DataSHIELD.checks> the checks are relatively slow. 
#' Default <DataSHIELD.checks> value is FALSE. 
#' 
#' If <force.colnames> is NULL column names are inferred from the names or column names
#' of the first object specified in the <x> argument.
#' The vector of column names must have the same number of elements as the columns in the output
#' object.
#' 
#' Server functions called: rbindDS. 
#' 
#' 
#' @param x a character vector with the  name of the objects to be combined.  
#' @param DataSHIELD.checks logical, if TRUE checks that all
#' input objects exist and are of an appropriate class. 
#' @param force.colnames can be NULL or a vector of characters which 
#' specifies column names of the output object. 
#' @param newobj a character string which provides the name for the output variable 
#' that is stored on the data servers.
#' @param datasources specifies the particular Opal object(s) to use. 
#' If the <datasources> argument is not specified the default set of Opals will be used.
#' @param notify.of.progress specifies if console output should be produce to indicate
#' progress. The default value for notify.of.progress is FALSE.
#' @return ds.rbind returns a matrix combining the rows of the R objects specified in the function. 
#' The created matrix is stored in the in the Opal servers. It also returns two messages with the name of <newobj>
#' that has been created in each data source and <DataSHIELD.checks> result. 
#' @examples 
#' #connecting to the Opal servers
#' 
#' require('DSI')
#' require('DSOpal')
#' require('dsBaseClient')
#'
#' builder <- DSI::newDSLoginBuilder()
#' builder$append(server = "study1", 
#'                url = "http://192.168.56.100:8080/", 
#'                user = "administrator", password = "datashield_test&", 
#'                table = "CNSIM.CNSIM1", driver = "OpalDriver")
#' builder$append(server = "study2", 
#'                url = "http://192.168.56.100:8080/", 
#'                user = "administrator", password = "datashield_test&", 
#'                table = "CNSIM.CNSIM2", driver = "OpalDriver")
#' builder$append(server = "study3",
#'                url = "http://192.168.56.100:8080/", 
#'                user = "administrator", password = "datashield_test&", 
#'                table = "CNSIM.CNSIM3", driver = "OpalDriver")
#' logindata <- builder$build()
#' 
#' connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D") #Log onto the remote Opal training servers
#' 
#' #Combining R objects by rows in the Opal servers 
#' 
#' ds.rbind(x = "D",    #data frames in the Opal servers to be conbined (see above the connection to the Opal servers)
#'          DataSHIELD.checks = FALSE,
#'          force.colnames = NULL,  # column names are inferred from the names or column names of the first object specified in the <x> argument
#'          newobj = "D.bind",      # name for the output object that is stored in the data servers
#'          datasources = connections, # All Opal servers are used (see above the connection to the Opal servers)
#'          notify.of.progress = FALSE)
#'          
#' datashield.logout(connections) #log out from the Opal servers
#' 
#' @author Paul Burton for DataSHIELD Development Team
#' @export
ds.rbind<-function(x=NULL,DataSHIELD.checks=FALSE,force.colnames=NULL,newobj=NULL,datasources=NULL,notify.of.progress=FALSE){

  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  if(is.null(x)){
    stop("Please provide a vector of character strings holding the name of the input elements!", call.=FALSE)
  }

  # the input variable might be given as column table (i.e. D$vector)
  # or just as a vector not attached to a table (i.e. vector)
  # we have to make sure the function deals with each case
  xnames <- extract(x)
  varnames <- xnames$elements
  obj2lookfor <- xnames$holders

if(DataSHIELD.checks)
{
  # check if the input object(s) is(are) defined in all the studies
  for(i in 1:length(varnames)){
    if(is.na(obj2lookfor[i])){
      defined <- isDefined(datasources, varnames[i])
    }else{
      defined <- isDefined(datasources, obj2lookfor[i])
    }
  }

  # call the internal function that checks the input object(s) is(are) of the same legal class in all studies.
  for(i in 1:length(x)){
    typ <- checkClass(datasources, x[i])
    if(!('data.frame' %in% typ) & !('matrix' %in% typ) & !('factor' %in% typ) & !('character' %in% typ) & !('numeric' %in% typ) & !('integer' %in% typ) & !('logical' %in% typ)){
      stop(" Only objects of type 'data.frame', 'matrix', 'numeric', 'integer', 'character', 'factor' and 'logical' are allowed.", call.=FALSE)
    }
  }
 }
  # check newobj not actively declared as null
  if(is.null(newobj)){
    newobj <- "rbind.newobj"
}


#CREATE THE VECTOR OF COLUMN NAMES
if(!is.null(force.colnames)){
colname.vector<-force.colnames
}else{

  colname.vector<-NULL
  class.vector<-NULL

for(j in 1:length(x))
{
testclass.var<-x[j]

calltext1<-paste0('class(', testclass.var, ')')
next.class <- DSI::datashield.aggregate(datasources, as.symbol(calltext1))
class.vector<-c(class.vector,next.class[[1]])
if (notify.of.progress)
    cat("\n",j," of ", length(x), " elements to combine in step 1 of 2\n")
}

for(j in 1:length(x))
{
test.df<-x[j]

if(class.vector[j]!="data.frame" && class.vector[j]!="matrix")
	{
	colname.vector<-c(colname.vector,test.df)
        if (notify.of.progress)
            cat("\n",j," of ", length(x), " elements to combine in step 2 of 2\n")
	}
else
	{
	calltext2<-paste0('colnames(', test.df, ')')
    df.names <- DSI::datashield.aggregate(datasources, as.symbol(calltext2))
	 colname.vector<-c(colname.vector,df.names[[1]])
         if (notify.of.progress)
              cat("\n",j," of ", length(x), " elements to combine in step 2 of 2\n")
        }
}
if (notify.of.progress)
    cat("\nBoth steps completed\n")

#CHECK FOR DUPLICATE NAMES IN COLUMN NAME VECTOR AND ADD ".k" TO THE kth REPLICATE
num.duplicates<-rep(0,length(colname.vector))

if(length(colname.vector)==1){
num.duplicates<-0
}else{
if(length(colname.vector>=2))
{
for(j in length(colname.vector):2)
{
	for(k in (j-1):1)
	{
	if(colname.vector[j]==colname.vector[k])
		{
		num.duplicates[j]<-num.duplicates[j]+1
		}
	}
}
}
}
num.duplicates.c<-as.character(num.duplicates)




for(m in 1:length(colname.vector))
{
if(num.duplicates[m]!="0")
	{

	colname.vector[m]<-paste0(colname.vector[m],".",num.duplicates.c[m])
	}
}
}

#prepare name vectors for transmission
 x.names.transmit<-paste(x,collapse=",")
 colnames.transmit<-paste(colname.vector,collapse=",")

 ###############################
 # call the server side function

	calltext <- call("rbindDS", x.names.transmit,colnames.transmit)


	DSI::datashield.assign(datasources, newobj, calltext)


#############################################################################################################
#DataSHIELD CLIENTSIDE MODULE: CHECK KEY DATA OBJECTS SUCCESSFULLY CREATED                                  #
																											#
#SET APPROPRIATE PARAMETERS FOR THIS PARTICULAR FUNCTION                                                 	#
test.obj.name<-newobj																					 	#
																											#
																											#
																											#
																											#
# CALL SEVERSIDE FUNCTION                                                                                	#
calltext <- call("testObjExistsDS", test.obj.name)													 	#
																											#
object.info<-DSI::datashield.aggregate(datasources, calltext)												 	#
																											#
# CHECK IN EACH SOURCE WHETHER OBJECT NAME EXISTS														 	#
# AND WHETHER OBJECT PHYSICALLY EXISTS WITH A NON-NULL CLASS											 	#
num.datasources<-length(object.info)																	 	#
																											#
																											#
obj.name.exists.in.all.sources<-TRUE																	 	#
obj.non.null.in.all.sources<-TRUE																		 	#
																											#
for(j in 1:num.datasources){																			 	#
	if(!object.info[[j]]$test.obj.exists){																 	#
		obj.name.exists.in.all.sources<-FALSE															 	#
		}																								 	#
	if(is.null(object.info[[j]]$test.obj.class) || object.info[[j]]$test.obj.class=="ABSENT"){														 	#
		obj.non.null.in.all.sources<-FALSE																 	#
		}																								 	#
	}																									 	#
																											#
if(obj.name.exists.in.all.sources && obj.non.null.in.all.sources){										 	#
																											#
	return.message<-																					 	#
    paste0("A data object <", test.obj.name, "> has been created in all specified data sources")		 	#
																											#
																											#
	}else{																								 	#
																											#
    return.message.1<-																					 	#
	paste0("Error: A valid data object <", test.obj.name, "> does NOT exist in ALL specified data sources")	#
																											#
	return.message.2<-																					 	#
	paste0("It is either ABSENT and/or has no valid content/class,see return.info above")				 	#
																											#
	return.message.3<-																					 	#
	paste0("Please use ds.ls() to identify where missing")												 	#
																											#
																											#
	return.message<-list(return.message.1,return.message.2,return.message.3)							 	#
																											#
	}																										#
																											#
	calltext <- call("messageDS", test.obj.name)															#
    studyside.message<-DSI::datashield.aggregate(datasources, calltext)											#
																											#
	no.errors<-TRUE																							#
	for(nd in 1:num.datasources){																			#
		if(studyside.message[[nd]]!="ALL OK: there are no studysideMessage(s) on this datasource"){			#
		no.errors<-FALSE																					#
		}																									#
	}																										#
																											#
																											#
	if(no.errors){																							#
	validity.check<-paste0("<",test.obj.name, "> appears valid in all sources")							    #
	return(list(is.object.created=return.message,validity.check=validity.check))						    #
	}																										#
																											#
if(!no.errors){																								#
	validity.check<-paste0("<",test.obj.name,"> invalid in at least one source. See studyside.messages:")   #
	return(list(is.object.created=return.message,validity.check=validity.check,					    		#
	            studyside.messages=studyside.message))			                                            #
	}																										#
																											#
#END OF CHECK OBJECT CREATED CORECTLY MODULE															 	#
#############################################################################################################

}
#ds.rbind
