#' 
#' @title Converts a numeric vector into a factor type
#' @description This function assigns a numeric vector into a factor type
#' @details Converts a numeric vector into a factor type which is represented either as a vector
#' or as a matrix of dummy variables dependending on the argument \code{fixed.dummy.vars}.
#' @param input.var.name the name of the variable that is to be converted to a factor
#' @param newobj.name the name of the new object. If this argument is set to NULL or not specified 
#' then the default name of the new variable is the name of the input variable with the suffixe '.f'.
#' @param forced.factor.levels the levels that the user wants to split the input variable. If this
#' argument is set to NULL (default) then a vector with all unique levels from all studies is created 
#' as a result of the output of 'asFactorDS1.b' server-side function. The 'asFactorDS1.b' function 
#' returns the levels of the input variable from each single study in ascending order if the levels 
#' are numeric or in alphabetic order if the levels are characters. Then the levels from each study 
#' are combined together and a vector with all unique levels is created.
#' @param fixed.dummy.vars a boolean that determines whether the new object is represented as a vector
#' or as a matrix with elements dummy variables indicating the factor level of each data point.
#' If this argument is set to FALSE (default) then the input variable is converted to a factor and
#' assigned as a vector. If is set to TRUE then the input variable is converted to a factor but presented
#' as a matrix of dummy variables. The matrix of dummy variables also depends on argument 
#' \code{baseline.level}. To understand how this matrix is created let's assume that we have the vector
#' (1, 2, 1, 3, 4, 4, 1, 3, 4, 5) of ten integer numbers. If we set the arqyment \code{fixed.dummy.vars}
#' to TRUE and the \code{baseline.level} to 1 which is the default value and the \code{forced.factor.levels}
#' equal to c(1,2,3,4,5) then the input vector is converted to the following matrix of dummy variables:     
#'       DV2 DV3 DV4 DV5
#'  [1,]   0   0   0   0
#'  [2,]   1   0   0   0
#'  [3,]   0   0   0   0
#'  [4,]   0   1   0   0
#'  [5,]   0   0   1   0
#'  [6,]   0   0   1   0
#'  [7,]   0   0   0   0
#'  [8,]   0   1   0   0
#'  [9,]   0   0   1   0
#' [10,]   0   0   0   1
#' For the same example if the \code{baseline.level} is set to be equal to 3 then the matrix is:
#'       DV1 DV2 DV4 DV5
#'  [1,]   1   0   0   0
#'  [2,]   0   1   0   0
#'  [3,]   1   0   0   0
#'  [4,]   0   0   0   0
#'  [5,]   0   0   1   0
#'  [6,]   0   0   1   0
#'  [7,]   1   0   0   0
#'  [8,]   0   0   0   0
#'  [9,]   0   0   1   0
#' [10,]   0   0   0   1
#' In the first instance the first row of the matrix has zeros in all entries indicating that the first data 
#' point belongs to level 1 (as the baseline level is equal to 1). The second row has 1 at the first column 
#' (column 'DV2') and zeros elsewhere, indicating that the second data point belongs to level 2. In the second
#' instance (second matrix) where the baseline level is equal to 3, the first row of the matrix has 1 at the 
#' first column (column 'DV1') and zeros elsewhere, indicating again that the first data point belongs to level 1.
#' Also as we can see the fourth row of the second matrix has all its elements equal to zero indicating that the
#' fourth data point belongs to level 3 (as the baseline level in that case is 3).  
#' @param baseline.level a number indicating the baseline level to be used in the creation of the matrix with
#' dummy variables. If the \code{fixed.dummy.vars} is set to FALSE then any value of baseline level is not taken
#' into account. If the \code{fixed.dummy.vars} is set to TRUE then the baseline level is used as explained above.
#' If the \code{baseline.level} is set to be equal to a value that is not one of the levels of the factor then a 
#' matrix of dummy variables is created having as many columns as the number of levels are. In that case in each row
#' there is a unique entry equal to 1 at a certain column indicating the level of each data point. So, for the 
#' above example where the vector has five levels, if we set the \code{baseline.level} equal to a value that does not
#' belong to those five levels (let's say for example \code{baseline.level=8}) the the matrix of dummy variables is:
#'       DV1 DV2 DV3 DV4 DV5
#'  [1,]   1   0   0   0   0
#'  [2,]   0   1   0   0   0
#'  [3,]   1   0   0   0   0
#'  [4,]   0   0   1   0   0
#'  [5,]   0   0   0   1   0
#'  [6,]   0   0   0   1   0
#'  [7,]   1   0   0   0   0
#'  [8,]   0   0   1   0   0
#'  [9,]   0   0   0   1   0
#' [10,]   0   0   0   0   1
#' The creation of a factor in the form of a matrix with dummy variables at different baseline levels is useful 
#' in survival analysis for example in piecewise exponential regression where the baseline hazard is different 
#' in different time intervals. For more details see the description of ds.lexis and ds.glm functions.
#' @param datasources a list of opal object(s) obtained after login in to opal servers; these objects hold 
#' also the data assign to R, as \code{dataframe}, from opal datasources. By default an internal function looks
#' for 'opal' objects in the environment and sets this parameter. 
#' @return all the unique levels of the converted variable and the tracer of the function
#' @export
#' @examples
#' \dontrun{
#' 
#' #  # load that contains the login details
#' #  logindata.VMs.em <- ds.createLogindata(110,110,110,table=c("SURVIVAL.EXPAND_WITH_MISSING1",
#' #                            "SURVIVAL.EXPAND_WITH_MISSING2","SURVIVAL.EXPAND_WITH_MISSING3"))
#' #  logindata.VMs.em <- logindata.VMs.em[1:3,]
#' #  opals.em <- opal::datashield.login(logins=logindata.VMs.em,assign=TRUE,symbol="EM")
#' #
#' #  ds.asNumeric("EM$time.id","TID")
#' #
#' #  Example 1 
#' #  ds.asFactor.o("TID","TID.f")
#' #  ds.class("TID.f") 
#' #  ds.table1D("TID.f")
#' #
#' #  Example 2
#' #  ds.asFactor.o("TID","TID.f2",forced.factor.levels=1:6)
#' #  ds.class("TID.f2")
#' #  ds.table1D("TID.f2")
#' #
#' #  Example 3
#' #  ds.asFactor.o("TID","TID.f3",forced.factor.levels=0:10)
#' #  ds.class("TID.f3")
#' #  ds.table1D("TID.f3")
#' #
#' #  Example 4
#' #  ds.asFactor.o("TID","TID.f4",forced.factor.levels=2:3)
#' #  ds.class("TID.f4")
#' #  ds.table1D("TID.f4")
#' #
#' #  Example 5
#' #  ds.asFactor.o("TID","TID.f5",forced.factor.levels=c(1,2,3,4,"a","h",5))
#' #  ds.class("TID.f5")
#' #  ds.table1D("TID.f5")
#' #
#' #  Example 6
#' #  ds.asFactor.o("TID","TID.mat1",fixed.dummy.vars=TRUE)
#' #  ds.class("TID.mat1")
#' #
#' #  Example 7
#' #  ds.asFactor.o("TID","TID.mat6",fixed.dummy.vars=TRUE,baseline.level=6)
#' #  ds.class("TID.mat6")
#' 
#' }
#' 
ds.asFactor.o <- function(input.var.name=NULL, newobj.name=NULL, forced.factor.levels=NULL, fixed.dummy.vars=FALSE,
                        baseline.level=1, datasources=NULL){
  
  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
     datasources <- findLoginObjects()
  }
  
  # check if user has provided the name of the column that holds the input variable
  if(is.null(input.var.name)){
    stop("Please provide the name of the variable that is to be converted to a factor e.g. 'varname'", call.=FALSE)
  }

  # check if user has provided the name of the input variable in a correct character format
  if(!is.character(input.var.name)){
    stop("Please provide the name of the variable that is to be converted to a factor in character format e.g. 'varname'", call.=FALSE)
  }

  # if no output variable specified then provide a default name
  if(is.null(newobj.name)){
    newobj.name <- paste0(input.var.name,".f")
  }

  #CALL THE FIRST SERVER SIDE FUNCTION (AN AGGREGATE FUNCTION)
  #TO DETERMINE ALL OF THE LEVELS REQUIRED
  calltext1 <- call("asFactorDS1.o", input.var.name)
  all.levels <- opal::datashield.aggregate(datasources, calltext1)
  
  numstudies <- length(datasources)

  all.levels.all.studies <- NULL

  for(j in 1:numstudies){
  all.levels.all.studies <- c(all.levels.all.studies,all.levels[[j]])
  }

  all.unique.levels <- as.character(unique(all.levels.all.studies))

  if(!is.null(forced.factor.levels)){
     all.unique.levels <- forced.factor.levels
  }

  all.unique.levels.transmit <- paste0(all.unique.levels, collapse=",")

  calltext2 <- call("asFactorDS2.o", input.var.name, all.unique.levels.transmit, fixed.dummy.vars, baseline.level)
  opal::datashield.assign(datasources, newobj.name, calltext2)

##########################################################################################################
#MODULE 5: CHECK KEY DATA OBJECTS SUCCESSFULLY CREATED                                                   #
																										 #
#SET APPROPRIATE PARAMETERS FOR THIS PARTICULAR FUNCTION                                                 #
test.obj.name<-newobj.name                                                                               #
                                                                                                         #
# CALL SEVERSIDE FUNCTION                                                                                #
calltext <- call("testObjExistsDS.o", test.obj.name)													 #
object.info<-opal::datashield.aggregate(datasources, calltext)												 #
																										 #
# CHECK IN EACH SOURCE WHETHER OBJECT NAME EXISTS														 #
# AND WHETHER OBJECT PHYSICALLY EXISTS WITH A NON-NULL CLASS											 #
num.datasources<-length(object.info)																	 #
																										 #
																										 #
obj.name.exists.in.all.sources<-TRUE																	 #
obj.non.null.in.all.sources<-TRUE																		 #
																										 #
for(j in 1:num.datasources){																			 #
	if(!object.info[[j]]$test.obj.exists){																 #
		obj.name.exists.in.all.sources<-FALSE															 #
		}																								 #
	if(object.info[[j]]$test.obj.class=="ABSENT"){														 #
		obj.non.null.in.all.sources<-FALSE																 #
		}																								 #
	}																									 #
																										 #
if(obj.name.exists.in.all.sources && obj.non.null.in.all.sources){										 #
																										 #
	return.message<-																					 #
    paste0("Data object <", test.obj.name, "> correctly created in all specified data sources")		 	 #
																										 #
	return(list(all.unique.levels=all.unique.levels,return.message=return.message))						 #
																										 #
	}else{																								 #
																										 #
    return.message.1<-																					 #
	paste0("Error: A valid data object <", test.obj.name, "> does NOT exist in ALL specified data sources")#
																										 #
	return.message.2<-																					 #
	paste0("It is either ABSENT and/or has no valid content/class,see return.info above")				 #
																										 #
	return.message<-list(return.message.1,return.message.2)												 #
																										 #
	return.info<-object.info																			 #
																										 #
return(list(all.unique.levels=all.unique.levels,return.info=return.info,return.message=return.message))	 #
																										 #
	}																									 #
#END OF MODULE 5																						 #
##########################################################################################################


}
#ds.asFactor.o
