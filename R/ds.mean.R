#'
#' @title Computes server-side vector statistical mean 
#' @description This function computes the statistical mean
#'  of a given server-side vector. 
#'
#' @details  This function is similar to the R function `mean`.
#' 
#' The function can carry out 3 types of analysis depending on
#' the argument `type`:\cr
#' (1) If `type` is set to `'combine'`, `'combined'`, 
#' `'combines'` or `'c'`, a global mean is calculated.\cr
#' (2) If `type` is set to `'split'`, `'splits'` or `'s'`,
#'  the mean is calculated separately for each study. \cr
#' (3) If `type` is set to `'both'` or `'b'`, 
#' both sets of outputs are produced.
#' 
#' If the argument `save.mean.Nvalid` is set to TRUE 
#'  study-specific means and `Nvalids`
#' as well as the global equivalents across all studies combined 
#' are saved in the server-side. 
#' Once the estimated means and `Nvalids`
#' are written into the server-side R environments, they can be used directly to centralize
#' the variable of interest around its global mean or its study-specific means. Finally,
#' the `isDefined` internal function checks whether the key variables have been created.
#' 
#' Server function called: `meanDS`
#' @param x a character specifying the name of a numerical vector.
#' @param type a character string that represents the type of analysis to carry out.
#' This can be set as `'combine'`, `'combined'`, `'combines'`,
#' `'split'`, `'splits'`, `'s'`,
#' `'both'` or `'b'`. 
#' For more information see **Details**. 
#' @param checks logical. If TRUE  optional checks of model
#' components will be undertaken. Default is FALSE to save time. 
#' It is suggested that checks
#' should only be undertaken once the function call has failed. 
#' @param save.mean.Nvalid logical. If TRUE generated values of the mean and 
#' the number of valid (non-missing) observations will be saved  on the data servers. 
#' Default FALSE. 
#' For more information see **Details**. 
#' @param datasources a list of [DSConnection-class()] 
#' objects obtained after login. If the `datasources` argument is not specified
#' the default set of connections will be used: see [datashield.connections_default()].
#' @return `ds.mean` returns to the client-side a list including: \cr
#' 
#' `Mean.by.Study`:  estimated mean, `Nmissing`
#' (number of missing observations), `Nvalid` (number of valid observations) and
#' `Ntotal` (sum of missing and valid observations) 
#' separately for each study (if `type = split` or `type = both`). \cr
#' `Global.Mean`: estimated mean, `Nmissing`, `Nvalid` and `Ntotal` 
#' across all studies combined (if `type = combine` or `type = both`). \cr
#' `Nstudies`: number of studies being analysed. \cr
#' `ValidityMessage`: indicates if the analysis was possible. \cr
#' 
#' If `save.mean.Nvalid` is set as TRUE, the objects 
#' `Nvalid.all.studies`, `Nvalid.study.specific`,
#' `mean.all.studies` and `mean.study.specific` are written to the server-side. 
#' 
#' @author DataSHIELD Development Team
#' @seealso `ds.quantileMean` to compute quantiles.
#' @seealso `ds.summary` to generate the summary of a variable.
#' @export
#' @examples
#' \dontrun{
#'
#'  ## Version 6, for version 5 see the Wiki
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
#'   #Calculate the mean of a vector in the server-side
#'   
#'   ds.mean(x = "D$LAB_TSC",
#'           type = "split",
#'           checks = FALSE,
#'           save.mean.Nvalid = FALSE,
#'           datasources = connections)
#'              
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(connections)
#' }
#'
ds.mean <- function(x=NULL, type='split', checks=FALSE, save.mean.Nvalid=FALSE, datasources=NULL){

  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }
  
  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }
  
  if(is.null(x)){
    stop("Please provide the name of the input object!", call.=FALSE)
  }

  # beginning of optional checks - the process stops and reports as soon as one check fails                                                                                          #
  if(checks){
    
    # check if the input object is defined in all the studies
    isDefined(datasources, x)

    # call the internal function that checks the input object is of the same class in all studies.
    typ <- checkClass(datasources, x)
   
    # the input object must be a numeric or an integer vector
    if(!('integer' %in% typ) & !('numeric' %in% typ)){
      stop("The input object must be an integer or a numeric vector.", call.=FALSE)
    } 
}

###################################################################################################
#MODULE: EXTEND "type" argument to include "both" and enable valid alisases                     #
if(type == 'combine' | type == 'combined' | type == 'combines' | type == 'c') type <- 'combine'   #
if(type == 'split' | type == 'splits' | type == 's') type <- 'split'                              #
if(type == 'both' | type == 'b' ) type <- 'both'                                                  #
if(type != 'combine' & type != 'split' & type != 'both'){                                        #
  stop('Function argument "type" has to be either "both", "combine" or "split"', call.=FALSE)     #
}  

  cally <- paste0("meanDS(", x, ")")
  ss.obj <- DSI::datashield.aggregate(datasources, as.symbol(cally))

  Nstudies <- length(datasources)
  ss.mat <- matrix(as.numeric(matrix(unlist(ss.obj),nrow=Nstudies,byrow=TRUE)[,1:4]),nrow=Nstudies)
  dimnames(ss.mat) <- c(list(names(ss.obj),names(ss.obj[[1]])[1:4]))

  ValidityMessage.mat <- matrix(matrix(unlist(ss.obj),nrow=Nstudies,byrow=TRUE)[,5],nrow=Nstudies)
  dimnames(ValidityMessage.mat) <- c(list(names(ss.obj),names(ss.obj[[1]])[5]))

  ss.mat.combined <- t(matrix(ss.mat[1,]))

  ss.mat.combined[1,1] <- (t(matrix(ss.mat[,3]))%*%ss.mat[,1])/sum(ss.mat[,3])
  ss.mat.combined[1,2] <- sum(ss.mat[,2])
  ss.mat.combined[1,3] <- sum(ss.mat[,3])
  ss.mat.combined[1,4] <- sum(ss.mat[,4])

  dimnames(ss.mat.combined) <- c(list("studiesCombined"),list(names(ss.obj[[1]])[1:4]))

  # IF save.mean.Nvalid==TRUE - KEY STUDY SPECIFIC STATISTICS ON APPROPRIATE DATA REPOSITORY SERVERS WITH ASSIGN FUNCTION
  if(save.mean.Nvalid==TRUE){

    for(j in 1:Nstudies){
      selected.conn <- datasources[j]
      mean.study.specific <- ss.mat[j,1]
      Nvalid.study.specific <- ss.mat[j,3]
      # SAVE VALIDITY MESSAGE
      DSI::datashield.assign(selected.conn, "mean.study.specific", as.symbol(mean.study.specific))
      DSI::datashield.assign(selected.conn, "Nvalid.study.specific", as.symbol(Nvalid.study.specific))
    }

    # SAVE KEY GLOBAL STATISTICS ON ALL DATA REPOSITORY SERVERS WITH ASSIGN FUNCTION
    mean.all.studies <- ss.mat.combined[1,1]
    Nvalid.all.studies <- ss.mat.combined[1,3]
    DSI::datashield.assign(datasources, "mean.all.studies", as.symbol(mean.all.studies))
    DSI::datashield.assign(datasources, "Nvalid.all.studies", as.symbol(Nvalid.all.studies))

#############################################################################
# MODULE 5: CHECK DATA OBJECTS SUCCESSFULLY CREATED                         #
  key.names <- extract("mean.all.studies")                                  #
  key.varname <- key.names$elements                                         #
  key.obj2lookfor <- key.names$holders                                      #
                                                                            #
  if(is.na(key.obj2lookfor)){                                               #
    key.defined <- isDefined(datasources, key.varname)                      #
  }else{                                                                    #
    key.defined <- isDefined(datasources, key.obj2lookfor)                  #
  }                                                                         #
                                                                            #
#if(key.defined==TRUE){                                                      #
#print("Data object <mean.all.studies> created successfully in all sources") #
#}                                                                           #
#############################################################################
}

#PRIMARY FUNCTION OUTPUT SUMMARISE RESULTS FROM
#AGGREGATE FUNCTION AND RETURN TO CLIENT-SIDE
  if (type=='split'){
    return(list(Mean.by.Study=ss.mat,Nstudies=Nstudies,ValidityMessage=ValidityMessage.mat))
  }

  if (type=="combine") {
    return(list(Global.Mean=ss.mat.combined,Nstudies=Nstudies,ValidityMessage=ValidityMessage.mat))
  }

  if (type=="both") {
    return(list(Mean.by.Study=ss.mat,Global.Mean=ss.mat.combined,Nstudies=Nstudies,ValidityMessage=ValidityMessage.mat))
  }

}
#ds.mean
