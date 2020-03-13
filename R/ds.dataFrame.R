#'
#' @title Generates a data frame object in several Opal servers 
#' @description Creates a data frame from its elemental components: pre-existing data frames;
#' single variables; matrices
#' @details  \code{ds.dataFrame} function creates a data frame by combining
#' pre-existing data.frames, matrices or variables.
#' 
#' The length of all component variables, and the number of rows 
#' of the  data frames or matrices must be the same.  The output 
#' data frame will have the same number of rows. 
#' 
#' Server functions called: dataFrameDS
#' 
#' @param x a character string which provides the name of the objects
#' to be combined.
#' @param row.names	NULL, integer or character string which provides the
#'  row names of the output data frame
#' @param check.rows if TRUE then the rows are checked for consistency of
#' length and names. Default is FALSE. 
#' @param check.names logical. If TRUE the colum names 
#' in the data frame are checked to ensure that are unique. Default is TRUE. 
#' @param stringsAsFactors logical. If true the character vectors are
#' converted to factors. Default TRUE.
#' @param completeCases logical. If TRUE rows with one or more 
#' missing values will be deleted from the output data frame.
#' Default is FALSE.
#' @param DataSHIELD.checks logical. If TRUE undertakes all DataSHIELD checks (time
#' consuming). Default FALSE. 
#' @param newobj a character string  which provides the name for the output data frame  
#' that is stored on the data servers. Default \code{dataframe.newobj}. 
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login. 
#' If the \code{datasources}
#' the default set of connections will be used: see \link{datashield.connections_default}.
#' @param notify.of.progress specifies if console output should be produce to indicate
#' progress. Default is FALSE.
#' @return \code{ds.dataFrame} returns the object specified by the \code{newobj} argument (or default name <df_new>).
#' which is written to the serverside. In addition, two validity messages are returned
#' indicating whether \code{newobj}  has been created in each data source and if so whether
#' it is in a valid form. If its form is not valid in at least one study - e.g. because
#' a disclosure trap was tripped and creation of the full output object was blocked -
#' ds.dataFrame() also returns any studysideMessages that can explain the error in creating
#' the full output object. As well as appearing on the screen at run time,if you wish to
#' see the relevant studysideMessages at a later date you can use the {ds.message}
#' function. If you type ds.message("newobj") it will print out the relevant
#' studysideMessage from any datasource in which there was an error in creating <newobj>
#' and a studysideMessage was saved. If there was no error and <newobj> was created
#' without problems no studysideMessage will have been saved and ds.message("newobj")
#' will return the message: "ALL OK: there are no studysideMessage(s) on this datasource".
#' @author DataSHIELD Development Team
#' @export
ds.dataFrame<-function(x=NULL,row.names=NULL,check.rows=FALSE,check.names=TRUE,stringsAsFactors=TRUE,completeCases=FALSE,DataSHIELD.checks=FALSE,newobj=NULL,datasources=NULL,notify.of.progress=FALSE){

  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  if(is.null(x)){
    stop("Please provide the name of the list that holds the input vectors!", call.=FALSE)
  }

  # create a name by default if user did not provide a name for the new variable
  if(is.null(newobj)){
    newobj <- "dataframe.newobj"
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

  # check newobj not actively declared as null
  if(is.null(newobj)){
    newobj <- "df_new"
  }
}

#CREATE THE VECTOR OF COLUMN NAMES
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


 ###############################
  # call the server side function
  #The serverside function dataFrameDS calls dsBase::dataFrameDS in dsBase repository
  if(is.null(row.names)){
    cally <-  paste0("dataFrameDS(list(",paste(x,collapse=","),"),",
                     'NULL',",", check.rows,",", check.names,
                     ",list(","'",paste(colname.vector,collapse="','"),"'","),"
                     ,stringsAsFactors,",",completeCases,")")
  }else{
    cally <-  paste0("dataFrameDS(list(",paste(x,collapse=","),"),",
                     "list(","'",paste(row.names,collapse="','"),"'","),",
                     check.rows,",", check.names,
                     ",list(","'",paste(colname.vector,collapse="','"),"'","),"
                     ,stringsAsFactors,",",completeCases,")")
  }
  DSI::datashield.assign(datasources, newobj, as.symbol(cally))


#############################################################################################################
#DataSHIELD CLIENTSIDE MODULE: CHECK KEY DATA OBJECTS SUCCESSFULLY CREATED                                  #
																											#
#SET APPROPRIATE PARAMETERS FOR THIS PARTICULAR FUNCTION                                                 	#
test.obj.name<-newobj																					 	#
																											#
#TRACER																									 	#
#return(test.obj.name)																					 	#
#}                                                                                   					 	#
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
#ds.dataFrame
