#' @title ds.dataFrameSubset.o calling dataFrameSubsetDS1.o and dataFrameSubsetDS2.o
#' @description Subsets a data frame by row or by column.
#' @details A data frame is a list of variables all with the same number of rows,
#' which is of class 'data.frame'. ds.dataFrameSubset.o will subset a 
#' pre-existing data.frame by specifying the values of a subsetting variable
#' (subsetting by row) or by selecting columns to keep or remove (subsetting
#' by column). When subsetting by row, the resultant subset must strictly be
#' as large or larger than the disclosure trap value nfilter.subset. If you
#' wish to keep all rows in the subset (e.g. if the primary plan is to subset by column
#' not by row) then V1.name can be used to specify a vector of the same length
#' as the data.frame to be subsetted in each study in which every
#' element is 1 and there are no NAs. Such a vector can be created as follows:
#' First identify a convenient numeric variable with no missing values (typically a numeric
#' individual ID) let us call it indID, which is equal in length to the data.frame
#' to be subsetted. Then use the ds.make.o() function with the call
#' ds.make.o('indID-indID+1','ONES'). This creates a vector of ones (called 'ONES')
#' in each source equal in length to the indID vector in that source. 
#' @param df.name a character string providing the name for the data.frame
#' to be sorted. 
#' @param V1.name A character string specifying the name of a subsetting vector
#' to which a Boolean operator will be applied to define the subset to be created. Note
#' if the plan is to subset by column using ALL rows, then <V1.name>
#' might, for example, specify a vector consisting all of ones (see 'details' for how
#' to create such a vector.
#' @param V2.name A character string specifying the name of the vector
#' or scalar to which the values in the vector specified by the argument <V1.name>
#' is to be compared. So, for example, if <V2.name>
#' is a scalar (e.g. '4')
#' and the <Boolean.operator> argument is '<=', the subset data.frame that is created
#' will include all rows that correspond to a value of 4 or less in the subsetting
#' vector specified by the <V1.name> argument. If <V2.name> specifies a vector
#' (which must be of strictly the same length as the vector specified by <V1.name>)
#' and the <Boolean.operator> argument is '==', the subset data.frame that is
#' created will include
#' all rows in which the values in the vectors specified by <V1.name> and <V2.name>
#' are equal. If you are subsetting by column and want to keep all rows in the final subset,
#' <V1.name> can be specified as indicating a "ONES" vector created as described (above)
#' under 'details', <V2.name> can be specified as the scalar "1" and the <Boolean operator>
#' argument can be specified as "=="
#' @param Boolean.operator A character string specifying one of six possible Boolean operators:
#' '==', '!=', '>', '>=', '<', '<='
#' @param keep.cols a numeric vector specifying the numbers of the columns to be kept in the
#' final subset when subsetting by column. For example: keep.cols=c(2:5,7,12) will keep
#' columns 2,3,4,5,7 and 12.
#' @param rm.cols a numeric vector specifying the numbers of the columns to be removed before
#' creating the final subset when subsetting by column. For example: rm.cols=c(2:5,7,12)
#' will remove columns 2,3,4,5,7 and 12.
#' @param keep.NAs logical, if TRUE any NAs in the vector holding the final Boolean vector
#' indicating whether a given row should be included in the subset will be converted into
#' 1s and so they will be included in the subset. Such NAs could be caused by NAs in
#' either <V1.name> or <V2.name>. If FALSE or NULL NAs in the final Boolean vector will
#' be converted to 0s and the corresponding row will therefore be excluded from the subset.
#' @param newobj This a character string providing a name for the subset
#' data.frame representing the primary output of the ds.dataFrameSubset.o() function.
#' This defaults to '<df.name>_subset' if no name is specified
#' where <df.name> is the first argument of ds.dataFrameSubset.o()
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
#' @return the object specified by the <newobj> argument (or default name '<df.name>_subset').
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

ds.dataFrameSubset.o<-function(df.name=NULL, V1.name=NULL, V2.name=NULL, Boolean.operator=NULL, keep.cols=NULL, rm.cols=NULL, keep.NAs=NULL, newobj=NULL, datasources=NULL, notify.of.progress=FALSE){
  
  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }
  
  # check if user has provided the name of the data.frame to be subsetted
  if(is.null(df.name)){
    stop("Please provide the name of the data.frame to be subsetted as a character string: eg 'xxx'", call.=FALSE)
  }
   
  # check if user has provided the name of the column or scalar that holds V1
  if(is.null(V1.name)){
    stop("Please provide the name of the column or scalar that holds V1 as a character string: eg 'xxx' or '3'", call.=FALSE)
  }

    # check if user has provided the name of the column or scalar that holds V2
  if(is.null(V2.name)){
    stop("Please provide the name of the column or scalar that holds V2 as a character string: eg 'xxx' or '3'", call.=FALSE)
  }

  # check if user has provided a Boolean operator in character format: eg '==' or '>=' or '<' or '!='
  if(is.null(Boolean.operator)){
    stop("Please provide a Boolean operator in character format: eg '==' or '>=' or '<' or '!='", call.=FALSE)
  }
  
  #if keep.NAs is set as NULL convert to FALSE as otherwise the call to datashield.assign will fail
  if(is.null(keep.NAs)){
  keep.NAs<-FALSE
  }

#convert Boolean operator to numeric

BO.n<-0
if(Boolean.operator == "=="){
   BO.n<-1
}

if(Boolean.operator == "!="){
   BO.n<-2
}

if(Boolean.operator == "<"){
   BO.n<-3
}

if(Boolean.operator == "<="){
   BO.n<-4
}

if(Boolean.operator == ">"){
   BO.n<-5
}

if(Boolean.operator == ">="){
   BO.n<-6
}

  # if no value spcified for output object, then specify a default
  if(is.null(newobj)){
    newobj <- paste0(df.name,"_subset")
  }

 if(!is.null(keep.cols)){
  keep.cols<-paste0(as.character(keep.cols),collapse=",")
 } 
 
if(!is.null(rm.cols)){
  rm.cols<-paste0(as.character(rm.cols),collapse=",")
 } 
  
  
  
    calltext1 <- call("dataFrameSubsetDS1.o", df.name, V1.name, V2.name, BO.n, keep.cols, rm.cols, keep.NAs=keep.NAs)
    return.warning.message<-opal::datashield.aggregate(datasources, calltext1)

    calltext2 <- call("dataFrameSubsetDS2.o", df.name, V1.name, V2.name, BO.n, keep.cols, rm.cols, keep.NAs=keep.NAs)
    opal::datashield.assign(datasources, newobj, calltext2)
	
 
    numsources<-length(datasources)
    for(s in 1:numsources){
	num.messages<-length(return.warning.message[[s]])
        if (notify.of.progress)
	{
            if(num.messages==1){
	        cat("\nSource",s,"\n",return.warning.message[[s]][[1]],"\n")
	    }else{
	        cat("\nSource",s,"\n")
		for(m in 1:(num.messages-1)){
		    cat(return.warning.message[[s]][[m]],"\n")
                }
                cat(return.warning.message[[s]][[num.messages]],"\n")
	    }
        }
    }

#############################################################################################################
#DataSHIELD CLIENTSIDE MODULE: CHECK KEY DATA OBJECTS SUCCESSFULLY CREATED                                  #
																											#
#SET APPROPRIATE PARAMETERS FOR THIS PARTICULAR FUNCTION                                                 	#
test.obj.name<-newobj																					 	#
																											#																											#
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
#ds.dataFrameSubset.o

