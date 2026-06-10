#' @title Converts a server-side numeric vector into a factor 
#' @description This function assigns a server-side numeric vector into a factor class.
#' @details Converts a numeric vector into a factor type which is represented either as a vector
#' or as a matrix of dummy variables depending on the argument \code{fixed.dummy.vars}.
#' The matrix of dummy variables also depends on the argument
#' \code{baseline.level}.
#'
#' ds.asFactor.R and its associated serverside functions asFactorDS1 and asFactorDS2
#' are to be used when you have variable that has up to 40 unique levels across
#' all sources combined. If one of the sources does not contain any subjects at a
#' particular level, that level will still be created as an empty category. In the
#' end all sources thus include a factor variable with consistent factor levels
#' across all sources - one level for every unique value that occurs in at least one
#' source. This is important when you wish to fit models using ds.glm because the factor
#' levels must be consistent across all studies or the model will not fit.
#'
#' But in order for this to be possible, all sources have to share all of the unique values
#' their source holds for the variable. This allows the client to create a single vector
#' containing all of the unique factor levels across ALL sources. But this is potentially
#' disclosive if there are too many levels. There are therefore two checks on the number of
#' levels in each source. One is simply a test of whether the number of levels exceeds
#' a value specified by the Roption value 'nfilter.max.levels' which is set by default
#' to 40, but the data custodian for the source can choose any alternative value he/she
#' chooses. The second test is of whether the levels are too dense: ie do the number of
#' levels exceed a specified proportion of the full length of the relevant vector
#' in the particular source. The max density is set by the Roption value 'nfilter.levels'
#' which takes the default value 0.33 but can again be modified by the data custodian.
#'
#' In combination, these two checks mean that if a factor has 35 levels in a given study
#' where the total length of the variable to be converted to a factor is 1000 individuals,
#' the ds.asFactor function will process that variable appropriately. But if it had had
#' 45 levels it would have been blocked by 'nfilter.max.levels' and if the total length of
#' the variable in that study had only been 70 subjects it would have been blocked by
#' the density criterion held in 'nfilter.levels'.
#'
#' If you have a factor with more than 40 levels in each source - perhaps most commonly
#' an ID of some sort that you need to provide as an argument to eg a tapply function.
#' Then you cannot use ds.asFactor. Typically in these circumstance you simply want
#' to create a factor that is appropriate for each source but you do not need to ensure
#' that all levels are consistent across all sources. In that case, you can use the
#' ds.asFactorSimple function which does no more than coerce a numeric or character
#' variable to a factor. Because you do not need to share unique factor levels
#' between sources, there is then no disclosure issue.
#'
#' To understand how the matrix of the dummy variable is created let's assume that we have the vector
#' \code{(1, 2, 1, 3, 4, 4, 1, 3, 4, 5)} of ten integer numbers. 
#' If we set the argument \code{fixed.dummy.vars = TRUE},
#' \code{baseline.level = 1} and \code{forced.factor.levels = c(1,2,3,4,5)}. 
#' The input vector is converted to the following matrix of dummy variables: 
#'
#' \tabular{rrrr}{
#'   \strong{DV2} \tab \strong{DV3} \tab \strong{DV4} \tab \strong{DV5} \cr
#'     0   \tab  0  \tab  0  \tab  0\cr
#'     1   \tab  0  \tab  0  \tab  0\cr
#'     0   \tab  0  \tab  0  \tab  0\cr
#'     0   \tab  1  \tab  0  \tab  0\cr
#'     0   \tab  0  \tab  1  \tab  0\cr
#'     0   \tab  0  \tab  1  \tab  0\cr
#'     0   \tab  0  \tab  0  \tab  0\cr
#'     0   \tab  1  \tab  0  \tab  0\cr
#'     0   \tab  0  \tab  1  \tab  0\cr
#'     0   \tab  0  \tab  0  \tab  1  
#' }
#'
#'  For the same example if the \code{baseline.level = 3} then the matrix is: 
#'
#'  \tabular{rrrr}{
#'    \strong{DV1} \tab \strong{DV2} \tab \strong{DV4} \tab \strong{DV5}  \cr
#'     1 \tab  0 \tab  0 \tab  0\cr
#'     0 \tab  1 \tab  0 \tab  0\cr
#'     1 \tab  0 \tab  0 \tab  0\cr
#'     0 \tab  0 \tab  0 \tab  0\cr
#'     0 \tab  0 \tab  1 \tab  0\cr
#'     0 \tab  0 \tab  1 \tab  0\cr
#'     1 \tab  0 \tab  0 \tab  0\cr
#'     0 \tab  0 \tab  0 \tab  0\cr
#'     0 \tab  0 \tab  1 \tab  0\cr
#'     0 \tab  0 \tab  0 \tab  1  
#' }
#'
#' In the first instance the first row of the matrix has zeros in all entries indicating 
#' that the first data point belongs to level 1 (as the baseline level is equal to 1). 
#' The second row has 1 at the first (\code{DV2}) column and zeros elsewhere,
#' indicating that the second data point belongs to level 2. 
#' In the second instance (second matrix) where the baseline level is equal to 3, 
#' the first row of the matrix has 1 at the
#' first (\code{DV1}) column and zeros elsewhere, 
#' indicating again that the first data point belongs to level 1.
#' Also as we can see the fourth row of the second matrix has all its elements equal 
#' to zero indicating that the
#' fourth data point belongs to level 3 (as the baseline level, in that case, is 3).
#'
#' If the \code{baseline.level} is set to be equal to a value 
#' that is not one of the levels of the factor then a
#' matrix of dummy variables is created having as many columns as the number of levels.
#' In that case in each row there is a unique entry equal to 1 at a 
#' certain column indicating the level of each data point. So, for the
#' above example where the vector has five levels
#' if we set the \code{baseline.level} equal to a value that does not
#' belong to those five levels (\code{baseline.level=8}) 
#' the matrix of dummy variables is:
#'
#' \tabular{rrrrr}{
#'        \strong{DV1}  \tab \strong{DV2} \tab \strong{DV3} \tab \strong{DV4} \tab \strong{DV5}\cr
#'         1   \tab  0  \tab 0   \tab 0   \tab 0\cr
#'         0   \tab  1  \tab 0   \tab 0   \tab 0\cr
#'         1   \tab  0  \tab 0   \tab 0   \tab 0\cr
#'         0   \tab  0  \tab 1   \tab 0   \tab 0\cr
#'         0   \tab  0  \tab 0   \tab 1   \tab 0\cr
#'         0   \tab  0  \tab 0   \tab 1   \tab 0\cr
#'         1   \tab  0  \tab 0   \tab 0   \tab 0\cr
#'         0   \tab  0  \tab 1   \tab 0   \tab 0\cr
#'         0   \tab  0  \tab 0   \tab 1   \tab 0\cr
#'         0   \tab  0  \tab 0   \tab 0   \tab 1  
#' }
#'
#'
#' Server functions called: \code{asFactorDS1} and \code{asFactorDS2}
#' @param input.var.name a character string which provides 
#' the name of the variable to be converted to a factor. 
#' @param newobj.name a character string that provides the name for the output variable 
#' that is stored on the data servers. Default \code{asfactor.newobj}. 
#' @param forced.factor.levels the levels that the user wants to split the input variable. 
#' If NULL (default) a vector with all unique levels from all studies are created. 
#' @param fixed.dummy.vars boolean. If TRUE the input variable is converted to a factor 
#' but presented as a matrix of dummy variables. 
#' If FALSE (default) the input variable
#' is converted to a factor and assigned as a vector. 
#' @param baseline.level an integer indicating the baseline level 
#' to be used in the creation of the matrix with dummy variables. 
#' If the \code{fixed.dummy.vars} is set to FALSE then any value of the baseline level is not taken
#' into account.
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} objects obtained after login. 
#' If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#' @return \code{ds.asFactor} returns the unique levels of the converted 
#' variable in ascending order and a validity 
#' message with the name of the created object on the client-side and 
#' the output matrix or vector in the server-side.  
#'
#' @examples
#' \dontrun{
#'
#'   ## Version 6, for version 5 see Wiki
#'   # Connecting to the Opal servers
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
#'   # Log onto the remote Opal training servers
#'   connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D") 
#'
#'   ds.asFactor(input.var.name = "D$PM_BMI_CATEGORICAL", 
#'               newobj.name = "fact.obj", 
#'               forced.factor.levels = NULL, #a vector with all unique levels 
#'                                            #from all studies is created
#'               fixed.dummy.vars = TRUE, #create a matrix of dummy variables
#'               baseline.level = 1,
#'               datasources = connections)#all the Opal servers are used, in this case 3 
#'                                         #(see above the connection to the servers) 
#'   ds.asFactor(input.var.name = "D$PM_BMI_CATEGORICAL", 
#'               newobj.name = "fact.obj", 
#'               forced.factor.levels = c(2,3), #the variable is split in 2 levels
#'               fixed.dummy.vars = TRUE, #create a matrix of dummy variables
#'               baseline.level = 1,
#'               datasources = connections[1])#only the first Opal server is used ("study1")
#'
#'    # Clear the Datashield R sessions and logout  
#'    datashield.logout(connections) 
#' }
#' @author DataSHIELD Development Team
#' @export
ds.asFactor <- function(input.var.name=NULL, newobj.name=NULL, forced.factor.levels=NULL, fixed.dummy.vars=FALSE,
                        baseline.level=1, datasources=NULL){

  # look for DS connections
  if(is.null(datasources)){
     datasources <- datashield.connections_find()
  }

  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
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
    newobj.name <- "asfactor.newobj"
  }

  #CALL THE FIRST SERVER SIDE FUNCTION (AN AGGREGATE FUNCTION)
  #TO DETERMINE ALL OF THE LEVELS REQUIRED
  calltext1 <- call("asFactorDS1", input.var.name)
  all.levels <- DSI::datashield.aggregate(datasources, calltext1)

  failed.levels <- FALSE
  for (levels in all.levels) {
    levels.prefix <- substring(levels, 1, 7)
    if (any("FAILED:" == levels.prefix))
      failed.levels <- TRUE
  }
  if (failed.levels) {
    stop("Failed to obtain levels: ", paste0(unlist(all.levels), collapse=", "), call. = TRUE)
  }

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

  calltext2 <- call("asFactorDS2", input.var.name, all.unique.levels.transmit, fixed.dummy.vars, baseline.level)
  DSI::datashield.assign(datasources, newobj.name, calltext2)

##########################################################################################################
#MODULE 5: CHECK KEY DATA OBJECTS SUCCESSFULLY CREATED                                                   #
																										 #
#SET APPROPRIATE PARAMETERS FOR THIS PARTICULAR FUNCTION                                                 #
test.obj.name<-newobj.name                                                                               #
                                                                                                         #
# CALL SEVERSIDE FUNCTION                                                                                #
calltext <- call("testObjExistsDS", test.obj.name)													 #
object.info<-DSI::datashield.aggregate(datasources, calltext)												 #
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
	if(is.null(object.info[[j]]$test.obj.class) || ("ABSENT" %in% object.info[[j]]$test.obj.class)){														 #
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
#ds.asFactor
