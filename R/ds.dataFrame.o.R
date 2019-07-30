#' 
#' @title ds.dataFrame.o calling dataFrameDS.o
#' @description Creates a data frame from its elemental components: pre-existing data frames;
#' single variables; matrices
#' @details A data frame is a list of variables all with the same number of rows with unique row
#' names, which is of class 'data.frame'. ds.dataFrame.o will create a data frame by combining
#' a series of elemental components which may be pre-existing data.frames, matrices or variables.
#' A critical requirement is that the length of all component variables, and the
#' number of rows of the component data.frames or matrices must all be the same. The output
#' data.frame will then have this same number of rows. ds.dataFrame.o calls the serverside
#' function dataFrameDS.o which is almost the same as the native R function data.frame()
#' and so several of the arguments are precisely the same as for data.frame() 
#' @param x This is a vector of character strings representing the names of the elemental
#' components to be combined. For example, the call:
#' ds.dataFrame.o(x=c('DF_input','matrix.m','var_age'),newobj='DF_output') will
#' combine a pre-existing data.frame called DF_input with a matrix and a variable
#' called var_age. The output will be the combined data.frame DF_output. As many
#' elemental components as needed may be combined in any order e.g. 3 data.frames,
#' 7 variables and 2 matrices. For convenience the x argument can alternatively
#' be specified in a two step procedure, the first being a call to
#' the native R environment on the client server:
#' x.components<-c('DF_input1','matrix.m','DF_input2', 'var_age'); 
#' ds.dataFrame.o(x=x.components,newobj='DF_output')
#' @param row.names	NULL or a single integer or character string specifying a
#' column to be used as row names, or a character or integer vector giving the
#' row names for the data frame.
#' @param check.rows if TRUE then the rows are checked for consistency of
#' length and names.
#' @param check.names logical. If TRUE then the names of the variables
#' in the data frame are checked to ensure that they are syntactically
#' valid variable names and are not duplicated.
#' If necessary they are adjusted (by make.names) so that they are.
#' As a slight modification to the standard data.frame() function in native
#' R, if any column names are duplicated, the second and subsequent
#' occurances are given the suffixes .1, .2 etc by ds.dataFrame.o and so
#' there are never any duplicates when check.names is invoked by the
#' serverside function dataFrameDS.o
#' @param stringsAsFactors logical: should character vectors be converted
#' to factors? The 'factory-fresh' default is TRUE.
#' @param completeCases logical. Default FALSE. If TRUE then any rows with
#' missing values
#' in any of the elemental components of the final output data.frame
#' will be deleted.
#' @param DataSHIELD.checks logical: If TRUE undertakes all DataSHIELD checks (time
#' consuming). Default FALSE.
#' @param newobj This a character string providing a name for the output
#' data.frame which defaults to 'df_new' if no name is specified.
#' @param datasources specifies the particular opal object(s) to use. If the <datasources>
#' argument is not specified the default set of opals will be used. The default opals
#' are called default.opals and the default can be set using the function
#' {ds.setDefaultOpals.o}. If the <datasources> is to be specified, it should be set without
#' inverted commas: e.g. datasources=opals.em or datasources=default.opals. If you wish to
#' apply the function solely to e.g. the second opal server in a set of three,
#' the argument can be specified as: e.g. datasources=opals.em[2].
#' If you wish to specify the first and third opal servers in a set you specify:
#' e.g. datasources=opals.em[c(1,3)]
#' @param notify.of.progress specifies if console output should be produce to indicate
#' progress. The default value for notify.of.progress is FALSE.
#' @return the object specified by the <newobj> argument (or default name <df_new>).
#' which is written to the serverside. In addition, two validity messages are returned
#' indicating whether <newobj> has been created in each data source and if so whether
#' it is in a valid form. If its form is not valid in at least one study - e.g. because
#' a disclosure trap was tripped and creation of the full output object was blocked -
#' ds.dataFrame.o() also returns any studysideMessages that can explain the error in creating
#' the full output object. As well as appearing on the screen at run time,if you wish to
#' see the relevant studysideMessages at a later date you can use the {ds.message.o}
#' function. If you type ds.message.o("newobj") it will print out the relevant
#' studysideMessage from any datasource in which there was an error in creating <newobj>
#' and a studysideMessage was saved. If there was no error and <newobj> was created
#' without problems no studysideMessage will have been saved and ds.message.o("newobj")
#' will return the message: "ALL OK: there are no studysideMessage(s) on this datasource".
#' @author DataSHIELD Development Team
#' @export
ds.dataFrame.o<-function(x=NULL,row.names=NULL,check.rows=FALSE,check.names=TRUE,stringsAsFactors=TRUE,completeCases=FALSE,DataSHIELD.checks=FALSE,newobj='df_new',datasources=NULL,notify.of.progress=FALSE){
  
  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }
  
  if(is.null(x)){
    stop("Please provide the name of the list that holds the input vectors!", call.=FALSE)
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
next.class <- opal::datashield.aggregate(datasources, as.symbol(calltext1))
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
    df.names <- opal::datashield.aggregate(datasources, as.symbol(calltext2))
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
  #The serverside function dataFrameDS.o calls dsBase::dataframeDS in dsBase repository
  if(is.null(row.names)){
    cally <-  paste0("dataFrameDS.o(list(",paste(x,collapse=","),"),", 
                     'NULL',",", check.rows,",", check.names,
                     ",list(","'",paste(colname.vector,collapse="','"),"'","),"
                     ,stringsAsFactors,",",completeCases,")")
  }else{
    cally <-  paste0("dataFrameDS.o(list(",paste(x,collapse=","),"),", 
                     "list(","'",paste(row.names,collapse="','"),"'","),", 
                     check.rows,",", check.names,
                     ",list(","'",paste(colname.vector,collapse="','"),"'","),"
                     ,stringsAsFactors,",",completeCases,")") 
  }
  opal::datashield.assign(datasources, newobj, as.symbol(cally))
  
 
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
calltext <- call("testObjExistsDS.o", test.obj.name)													 	#
																											#
object.info<-opal::datashield.aggregate(datasources, calltext)												 	#
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
	if(object.info[[j]]$test.obj.class=="ABSENT"){														 	#
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
	calltext <- call("messageDS.o", test.obj.name)															#
    studyside.message<-opal::datashield.aggregate(datasources, calltext)											#
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
#ds.dataFrame.o

