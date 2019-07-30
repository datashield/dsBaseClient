#' @title ds.merge.o calling assign function mergeDS.o
#' @description merges (links) two data.frames together based on common
#' values in defined vectors in each data.frame
#' @details For further information see details of the native R function {merge}.
#' In choosing which variables to use to merge/link the data.frames the native
#' R function {merge} is very flexible. For example, you can choose to merge
#' using all vectors that appear in both data.frames. However, for ds.merge.o
#' in DataSHIELD it is required that all the vectors which dictate the merging
#' are explicitly identified for both data.frames using the <by.x.names> and 
#' <by.y.names> arguments
#' @param x.name, the name of the first data.frame to be merged specified in
#' inverted commas. For example: x.name='dfx.name'. Native R refers to
#' the first data.frame as x and the second as y.
#' @param y.name, the name of the second data.frame to be merged specified in
#' inverted commas. For example: y.name='dfy.name'. Native R refers to
#' the first data.frame as x and the second as y.
#' @param by.x.names the name of a single variable or a vector of names of multiple variables
#' containing the IDs or other data on which data.frame x is to be merged/linked
#' to data.frame y. Names must be specified in inverted commas.
#' For example: by.x.names='individual.ID' or
#' by.x.names=c('year.of.birth', 'month.of.birth', 'day.of.birth', 'surname')
#' @param by.y.names the name of a single variable or a vector of names of multiple variables
#' containing the IDs or other data on which data.frame y is to be merged/linked
#' to data.frame x. Names must be specified in inverted commas.
#' For example: by.y.names='individual.ID' or
#' by.y.names=c('year.of.birth', 'month.of.birth', 'day.of.birth', 'surname')
#' @param all.x	logical, if TRUE, then extra rows will be added to the output,
#' one for each row in x that has no matching row in y. These rows will have NAs in those
#' columns that are usually filled with values from y. Default is FALSE, so that only
#' rows with data from both x and y are included in the output.
#' @param all.y	logical, if TRUE, then extra rows will be added to the output,
#' one for each row in y that has no matching row in x. These rows will have NAs in those
#' columns that are usually filled with values from x. Default is FALSE, so that only
#' rows with data from both x and y are included in the output.
#' @param sort logical, if TRUE the merged result should be sorted on elements
#' in the by.x.names and by.y.names columns. Default = TRUE.
#' @param suffixes a character vector of length 2 specifying the suffixes to be used for
#' making unique common column names in the two input data.frames
#' when they both appear in the merged data.frame. This avoids ambiguity in the
#' source of columns that are not used for merging. Default is .x for vector names in
#' the first data.frame and .y for vector names in the second data.frame 
#' @param no.dups logical, indicating that suffixes are appended in more cases to
#' rigorously avoid duplicated column names in the merged data.frame. Default TRUE
#' but was apparently implicitly FALSE before R version 3.5.0.
#' @param incomparables, values which cannot be matched. See 'match' in help
#' for Native R {merge} function. This is intended to be used for merging on
#' one column, so these are incomparable values of that column.
#' @param newobj the name of the merged data.frame. If this argument is set
#' to NULL, the name of the merged data.frame is defaulted to 'x.name_y.name'
#' where x.name is the name of the first input data.frame
#' specified as the <x.name> argument and y.name
#' is the name of the second input data.frame specified as the <y.name> argument.
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
#' @return the merged data.frame specified by the <newobj> argument (or by default 'x.name_y.name'
#' if the <newobj> argument is NULL) which is written to the serverside. In addition,
#' two validity messages are returned to the clientside
#' indicating whether <newobj> has been created in each data source and if so whether
#' it is in a valid form. If its form is not valid in at least one study there may
#' be a studysideMessage that can explain the error in creating
#' the full output object. As well as appearing on the screen at run time,if you wish to
#' see the relevant studysideMessages at a later date you can use the {ds.message.o}
#' function. If you type ds.message.o(<newobj>) it will print out the relevant
#' studysideMessage from any datasource in which there was an error in creating <newobj>
#' and a studysideMessage was saved. If there was no error and <newobj> was created
#' without problems no studysideMessage will have been saved and ds.message.o(<newobj>)
#' will return the message: "ALL OK: there are no studysideMessage(s) on this datasource".
#' @author Amadou Gaye, Paul Burton, for DataSHIELD Development Team
#' @export


ds.merge.o = function(x.name=NULL,y.name=NULL, by.x.names=NULL, by.y.names=NULL,all.x=FALSE,all.y=FALSE,
			 sort=TRUE, suffixes = c(".x",".y"), no.dups=TRUE, incomparables=NULL, newobj=NULL, datasources=NULL){
  
  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }
 
#dataframe names 
  if(is.null(x.name)){
    stop("Please provide the name (eg 'name1') of first dataframe to be merged (called x) ", call.=FALSE)
  }
  
  if(is.null(y.name)){
    stop("Please provide the name (eg 'name2') of second dataframe to be merged (called y) ", call.=FALSE)
  }

#names of columns to merge on (may be more than one)
  if(is.null(by.x.names)){
    stop("Please provide the names of columns in x dataframe on which to merge (eg c('id', 'time'))", call.=FALSE)
	}
	
  if(is.null(by.y.names)){
    stop("Please provide the names of columns in y dataframe on which to merge (eg c('id', 'time'))", call.=FALSE)
	}
	
	#make transmittable via parser
    by.x.names.transmit <- paste(by.x.names,collapse=",")
    by.y.names.transmit <- paste(by.y.names,collapse=",")

#suffixes
  if(is.null(suffixes)){
    stop("Please provide the suffixes to append to disambiguate duplicate column names  (default = c('.x','.y/))", call.=FALSE)
  }
	#make transmittable via parser
    suffixes.transmit <- paste(suffixes,collapse=",")
	

  # create a name by default if user did not provide a name for the new variable
  if(is.null(newobj)){
    newobj <- paste0(x.name,"_",y.name)
  }

    # call the server side function


	calltext <- call("mergeDS.o", x.name, y.name, by.x.names.transmit, by.y.names.transmit, all.x, all.y,
			 sort, suffixes.transmit, no.dups, incomparables)

	opal::datashield.assign(datasources, newobj, calltext)


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
# ds.merge.o


