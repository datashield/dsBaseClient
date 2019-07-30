#' @title ds.matrix calling assign function matrixDS
#' @description Creates a matrix A on the serverside 
#' @details Similar to the {matrix()} function in native R. Creates a matrix
#' with dimensions specified by <nrows.scalar> and <ncols.scalar> arguments
#' and assigns the values of all its elements based on the <mdata> argument 
#' @param mdata specifies the elements of the matrix to be created. If it
#' is a vector it should usually be the same length as the total number of elements
#' in the matrix and these will fill the matrix column by column or
#' row by row depending whether the argument <byrow> is FALSE (default) or TRUE.
#' If the mdata vector is not the same length as the total number of elements
#' in the matrix to be created, the values in mdata will be used repeatedly until
#' all elements in the matrix are full. 
#' If mdata is a scalar, all elements in the matrix will take that value.
#' The <mdata> argument can be specified either as a character string specifying
#' the name of a serverside scalar or vector,
#' or a numeric value (or numeric object) representing a scalar specified from the
#' clientside. Zeros, negative values and NAs are all allowed.
#' @param from a character string specifying the source and nature of <mdata>.
#' Can only take values "serverside.vector", "serverside.scalar" or "clientside.scalar"
#' This argument must be specified - NULL values not permitted. Defaults to
#' "clientside.scalar"
#' @param nrows.scalar specifies the number of rows in the matrix to be created.
#' It can be a character string specifying the name of a serverside scalar: e.g.
#' if a serverside scalar named "ss.scalar" exists and holds the value 23,
#' then by specifying nrows.scalar="ss.scalar", the matrix to be created will
#' have 23 rows. Alternatively it can be specified as
#' a numeric value (or numeric object) representing a scalar specified from the
#' clientside: e.g. nrows.scalar=14 or equivalently
#' scalar.value<- 14; nrows.scalar=scalar.value. This will create a matrix
#' with 14 rows. A zero, negative, NULL or missing value is not permitted
#' @param ncols.scalar specifies the number of columns in the matrix to be created.
#' It can be a character string specifying the name of a serverside scalar: e.g.
#' if a serverside scalar named "ss.scalar" exists and holds the value 23,
#' then by specifying ncols.scalar="ss.scalar", the matrix to be created will
#' have 23 columns. Alternatively it can be specified as
#' a numeric value (or numeric object) representing a scalar specified from the
#' clientside: e.g. ncols.scalar=14 or equivalently
#' scalar.value<- 14; ncols.scalar=scalar.value. This will create a matrix
#' with 14 columns. A zero, negative, NULL or missing value is not permitted
#' @param byrow a logical value specifying whether, when <mdata> is a vector,
#' the matrix created should be filled row by row (byrow=TRUE) i.e.
#' starting at the first element of
#' first row, filling that row and then moving to the first element of the second
#' row and filling that row etc until all elements of the matrix are full or
#' column by column (byrow=FALSE). Default = FALSE.
#' @param dimnames A dimnames attribute for the matrix: NULL or a list of length 2 giving
#' the row and column names respectively. An empty list is treated as NULL,
#' and a list of length one as row names only. 
#' @param newobj A character string specifying the name of the matrix to which the output
#' is to be written. If no <newobj> argument is specified or it is NULL
#' the output matrix names defaults to "new_matrix"
#' @param datasources specifies the particular opal object(s) to use. If the <datasources>
#' argument is not specified the default set of opals will be used. The default opals
#' are called default.opals and the default can be set using the function
#' {ds.setDefaultOpals.o}. If the <datasources> is to be specified, it should be set without
#' inverted commas: e.g. datasources=opals.em or datasources=default.opals. If you wish to
#' apply the function solely to e.g. the second opal server in a set of three,
#' the argument can be specified as: e.g. datasources=opals.em[2].
#' If you wish to specify the first and third opal servers in a set you specify:
#' e.g. datasources=opals.em[c(1,3)]
#' @return the object specified by the <newobj> argument (or default name "new_matrix")
#' which is written to the serverside. In addition, two validity messages are returned
#' indicating whether <newobj> has been created in each data source and if so whether
#' it is in a valid form. If its form is not valid in at least one study - e.g. because
#' a disclosure trap was tripped and creation of the full output object was blocked -
#' ds.matrix also returns any studysideMessages that can explain the error in creating
#' the full output object. As well as appearing on the screen at run time,if you wish to
#' see the relevant studysideMessages at a later date you can use the {ds.message.o}
#' function. If you type ds.message.o("newobj") it will print out the relevant
#' studysideMessage from any datasource in which there was an error in creating <newobj>
#' and a studysideMessage was saved. If there was no error and <newobj> was created
#' without problems no studysideMessage will have been saved and ds.message.o("newobj")
#' will return the message: "ALL OK: there are no studysideMessage(s) on this datasource".
#' @author Paul Burton for DataSHIELD Development Team
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' ds.matrix(mdata=-13,from="clientside.scalar", nrows.scalar=3,ncols.scalar=8,newobj="cs.block")
#' 
#' ds.matrix(NA,from="clientside.scalar", nrows.scalar=4,ncols.scalar=5,newobj="cs.block.NA")
#' 
#' clientside.input.scalar<-837
#' ds.matrix(clientside.input.scalar,from="clientside.scalar", nrows.scalar=7,ncols.scalar=3,
#'           newobj="cs.block.input")
#' 
#' clientside.input.nrows.scalar<-11
#' ds.matrix(clientside.input.scalar,from="clientside.scalar",
#'           nrows.scalar=clientside.input.nrows.scalar,ncols.scalar=3,
#'           newobj="cs.block.input.nrows")
#' 
#' ds.rUnif.o(samp.size=1,min=0.5,max=10.5,newobj="block.scalar",seed.as.integer=761728,
#'            force.output.to.k.decimal.places = 0)
#' 
#' ds.matrix("block.scalar",from="serverside.scalar", nrows.scalar=9,ncols.scalar=7,
#'           newobj="ss.block")
#' 
#' ds.make.o("log(block.scalar*(-1))","block.scalar.NA")
#' 
#' ds.matrix("block.scalar.NA",from="serverside.scalar", nrows.scalar=9,ncols.scalar=7,
#'           newobj="ss.block.NA")
#' 
#' ds.matrix("block.scalar",from="serverside.scalar", nrows.scalar="block.scalar",
#'           ncols.scalar="block.scalar",newobj="ss.block.square")
#' 
#' ds.rUnif.o(samp.size=45,min=-10.5,max=10.5,newobj="ss.vector",seed.as.integer=8321,
#'            force.output.to.k.decimal.places = 0)
#' ds.matrix("ss.vector",from="serverside.vector", nrows.scalar=5,ncols.scalar=9,
#'           newobj="sv.block")
#' 
#' ds.rUnif.o(samp.size=5,min=-10.5,max=10.5,newobj="ss.vector.5",seed.as.integer=551625,
#'            force.output.to.k.decimal.places = 0)
#' ds.matrix("ss.vector.5",from="serverside.vector", nrows.scalar=5,ncols.scalar=9,
#'           newobj="sv.block.5")
#' 
#' ds.rUnif.o(samp.size=9,min=-10.5,max=10.5,newobj="ss.vector.9",seed.as.integer=5575,
#'            force.output.to.k.decimal.places = 0)
#' ds.matrix("ss.vector.9",from="serverside.vector", nrows.scalar=5,ncols.scalar=9,byrow=TRUE,
#'           newobj="sv.block.9")
#' 
#' ds.matrix("ss.vector.9",from="serverside.vector", nrows.scalar=5,ncols.scalar=9,
#'           newobj="sv.block.9.ragged")
#' 
#' ds.rUnif.o(samp.size=12,min=-10.5,max=10.5,newobj="ss.vector.12",seed.as.integer=778172,
#'            force.output.to.k.decimal.places = 0)
#' 
#' ds.matrix("ss.vector.12",from="serverside.vector", nrows.scalar=5,ncols.scalar=9,
#'           newobj="sv.block.12.ragged")
#' 
#' ds.recodeValues.o("ss.vector", c(-10),c(NA),newobj="ss.vector.NA")
#' ds.matrix("ss.vector.NA",from="serverside.vector", nrows.scalar=5,ncols.scalar=9,
#'           newobj="sv.block.NA")
#' 
#' ds.matrix("ss.vector.NA",from="serverside.vector", nrows.scalar=5,ncols.scalar=9,
#'           byrow=TRUE,newobj="sv.byrow.block")
#' 
#' ds.matrix(NA, nrows.scalar=7,ncols.scalar=6,newobj="empty.matrix")
#' 
#' ds.matrix("ss.vector.9",from="serverside.vector", nrows.scalar=5,ncols.scalar=9,byrow=TRUE,
		  #' dimnames=list(c("a","b","c","d","e")),newobj="sv.block.9.dimnames1")
#' 
#' ds.matrix("ss.vector.9",from="serverside.vector", nrows.scalar=5,ncols.scalar=9,byrow=TRUE,
          #' dimnames=list(c("a","b","c","d","e"),c(10*(9:1))),newobj="sv.block.9.dimnames12")
#' 
#' #No specification of newobj
#' ds.matrix("ss.vector.9",from="serverside.vector", nrows.scalar=5,ncols.scalar=9,byrow=TRUE,
          #' dimnames=list(c("a","b","c","d","e"),c(10*(9:1))))
#' }
#'
ds.matrix<-function(mdata = NA, from="clientside.scalar",nrows.scalar=NULL, ncols.scalar=NULL, byrow = FALSE,
                   dimnames = NULL, newobj=NULL, datasources=NULL){
  
 
  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }
  
  # check if a value has been provided for mdata
  if(is.null(mdata)){
    return("Error: mdata must be a character string, a numeric vector or a scalar")
  }

  # if no value spcified for output object, then specify a default
  if(is.null(newobj))
  {
    newobj <- "new_matrix"
  }

  #process mdata to make transmittable depending on <from>
  #Check that valid from has been specified
  if(from!="serverside.vector"&&from!="serverside.scalar"&&from!="clientside.scalar")
  {
  cat("            FAILED: <from> must be specified as one of the following - 'serverside.vector',
        'serverside.scalar', 'clientside.scalar'\n\n")
  return('Please respecify')
  }

  if(from=="serverside.vector"||from=="serverside.scalar")
  {
  mdata.transmit<-mdata
  }
 
  
  if(from=="clientside.scalar")
  {
  mdata.transmit<-paste0(as.character(mdata),collapse=",")
  }

  #process <nrows> and <ncols> to make transmittable depending on specified from
  #in principle acceptable values of <nrows> and <ncols> cannot be negative

  if(is.null(nrows.scalar))
  {
  nrows.scalar<-c(-9)
  }  

  nrows.transmit<-paste0(as.character(nrows.scalar),collapse=",")
 
  if(is.null(ncols.scalar))
  {
  ncols.scalar<-c(-9)
  }  
  ncols.transmit<-paste0(as.character(ncols.scalar),collapse=",")

# CALL THE MAIN SERVER SIDE FUNCTION
  calltext <- call("matrixDS", mdata.transmit, from, nrows.transmit, ncols.transmit, byrow, dimnames)
  opal::datashield.assign(datasources, newobj, calltext)





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
#ds.matrix
