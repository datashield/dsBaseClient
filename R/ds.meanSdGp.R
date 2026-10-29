#'
#' @title  Computes the mean and standard deviation across groups defined by one factor
#' @description This function calculates the mean and SD of a continuous variable for each class of
#' a single factor.
#' @details This function calculates the mean, standard deviation (SD), N (number of observations)
#' and the standard error of the mean (SEM) of a continuous variable broken down into subgroups
#' defined by a single factor. 
#' 
#' \code{ds.meanSdGp} does not subset the data; it only calculates and reports the statistics.
#' To create physical subsets use \code{ds.dataFrameSubset}.
#'
#' Only one categorising factor is allowed. If you have two factors (e.g. sex with two levels \code{[0,1]} and \code{BMI.categorical} with
#' three levels \code{[1,2,3]}) you simply need to create a new factor that combines the two together in a
#' way that gives each combination of levels a different value in the new factor. So, in the
#' example given, the calculation \code{newfactor = (3*sex) + BMI} gives you six values: \cr
#' (1) \code{sex = 0} and \code{BMI = 1} -> \code{newfactor = 1} \cr
#' (2) \code{sex = 0} and \code{BMI = 2} -> \code{newfactor = 2} \cr
#' (3) \code{sex = 0} and \code{BMI = 3} -> \code{newfactor = 3} \cr
#' (4) \code{sex = 1} and \code{BMI = 1} -> \code{newfactor = 4} \cr
#' (5) \code{sex = 1} and \code{BMI = 2} -> \code{newfactor = 5} \cr
#' (6) \code{sex = 1} and \code{BMI = 3} -> \code{newfactor = 6} \cr
#' 
#' The sample size reported for each group is the number of observations that are non-missing
#' for both the continuous variable and the factor, because only those contribute to the group
#' means and SDs. Missing counts are reported only overall, as \code{Ntotal} and \code{Nmissing}.
#'
#' Depending on the variable \code{type} can be carried out different analysis:\cr
#' (1) \code{"combine"}: a pooled table of results is generated. \cr
#' (2) \code{"split"} a table of results is generated for each study. \cr
#' (3) \code{"both"} both sets of outputs are produced.
#' 
#' Server function called: \code{meanSdGpDS}
#' @param x a character string specifying the name of a numeric continuous
#' variable.
#' @param y  a character string specifying the name of a categorical
#' variable of class factor.
#' @param type a character string that represents the type of analysis to carry out.
#' This can be set as: \code{"combine"}, \code{"split"} or \code{"both"}. 
#' Default \code{"both"}. 
#' For more information see \strong{Details}.  
#' @template classConsistencyCheckTrue
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} 
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#' @return \code{ds.meanSdGp} returns to the client-side the mean, SD, Nvalid and SEM combined
#' across studies and/or separately for each study, depending on the argument \code{type}. 
#' 
#' @author DataSHIELD Development Team
#' @author Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
#' @seealso \code{\link{ds.dataFrameSubset}} to subset a data frame by rows or columns.
#' @export
#' @examples
#' \dontrun{
#' 
#'  ## Version 6, for version 5 see the Wiki
#'   
#'   # connecting to the Armadillo/Opal servers
#' 
#'   require('DSI')
#'   require('DSMolgenisArmadillo')
#'   require('DSOpal')
#'   require('dsBaseClient')
#'
#'   armadillo_url <- "http://192.168.56.100:8080/"
#'   token <- armadillo.get_credentials(armadillo_url)
#'   builder <- DSI::newDSLoginBuilder()
#'   builder$append(server = "study1", 
#'                  url = armadillo_url, 
#'                  token = token@access_token, 
#'                  table = "datashield/survival/EXPAND_NO_MISSING1", driver = "ArmadilloDriver")
#'   builder$append(server = "study2", 
#'                  url = "https://192.168.56.100:8443/", 
#'                  user = "administrator", password = "datashield_test&", 
#'                  table = "SURVIVAL.EXPAND_NO_MISSING2", driver = "OpalDriver",
#'                  options = "list(ssl_verifyhost=0, ssl_verifypeer=0)")
#'   logindata <- builder$build()
#'   
#'   connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D") 
#'
#'
#'   #Example 1: Calculate the mean, SD, Nvalid and SEM of the continuous variable age.60 (age in
#'   #years centralised at 60), broken down by time.id (a six level factor relating to survival time)
#'   #and report the pooled results combined across studies.
#'  
#'   ds.meanSdGp(x = "D$age.60",
#'               y = "D$time.id",
#'               type = "combine",
#'               datasources = connections)
#'               
#'   #Example 2: Calculate the mean, SD, Nvalid and SEM of the continuous variable age.60 (age in
#'   #years centralised at 60), broken down by time.id (a six level factor relating to survival time)
#'   #and report both study-specific results and the pooled results combined across studies.
#'   #Save the returned output to msg.b.
#'   
#'   ds.meanSdGp(x = "D$age.60",
#'               y = "D$time.id",
#'               type = "both",
#'               datasources = connections)  
#'              
#'   # clear the Datashield R sessions and logout
#'   datashield.logout(connections)
#' }
#'
ds.meanSdGp <- function(x=NULL, y=NULL, type='both', datasources=NULL, classConsistencyCheck=TRUE){

  datasources <- .set_datasources(datasources)

  if(is.null(x)){
    stop("Please provide the name of the input vector!", call.=FALSE)
  }

  if(is.null(y)){
    stop("Please provide the name of the input vector!", call.=FALSE)
  }

  # names of the studies
  stdnames <- names(datasources)
  
  # variable names
  xnames <- extract(x)
  ynames <- extract(y)
  xvarname <- xnames$elements
  yvarname <- ynames$elements

  # call the server side function that calculates mean and standard deviation
  # by group in each study
  cally <- call("meanSdGpDS", x, y)
  output <- DSI::datashield.aggregate(datasources, cally)

  if(classConsistencyCheck){
    # check the summarised variable (x) and grouping factor (y) each have the
    # same class in all studies
    .checkClassConsistency(output, field = "class.x", object_name = x)
    .checkClassConsistency(output, field = "class.index", object_name = y)
  }

  numsources <- length(output)

  all.tables.valid<-1
  for(i in 1:numsources){
    if(unlist(output[[i]]$Table_valid==FALSE)) all.tables.valid <- all.tables.valid-1
  }

  if(all.tables.valid<1){
    warning.message<-"At least 1 cell count is 1-nfilter, please regroup"
    return(warning.message)
  }

  if(all.tables.valid==1){
    # COMBINE OR BOTH
    if(type!="split"){
	  numsources <- length(output)
	  mean.matrix <- NULL
	  sd.matrix <- NULL
	  n.matrix <- NULL
	  Nvalid <- 0
	  Nmissing <- 0
	  Ntotal <- 0

	  for(j in 1:numsources){
		mean.matrix <- rbind(mean.matrix,as.numeric(unlist(output[[j]][2])))
		sd.matrix <- rbind(sd.matrix,as.numeric(unlist(output[[j]][3])))
		n.matrix <- rbind(n.matrix,as.numeric(unlist(output[[j]][4])))
		Nvalid <- Nvalid+as.numeric(unlist(output[[j]][5]))
		Nmissing <- Nmissing+as.numeric(unlist(output[[j]][6]))
		Ntotal <- Ntotal+as.numeric(unlist(output[[j]][7]))
	  }
	  var.matrix <- sd.matrix^2

      nsum.vector <- rep(1,numsources)
      # Calculate weighted means across studies in each group
      mean.gp <- (diag(t(mean.matrix)%*%n.matrix))/(t(n.matrix)%*%nsum.vector)

      # Calculate weighted SDs across studies in each group
      var.gp <- (diag(t(var.matrix)%*%n.matrix))/(t(n.matrix)%*%nsum.vector)
      SD.gp <- sqrt(var.gp)
      N.gp <- (t(n.matrix) %*% nsum.vector)
      SEM.gp <- SD.gp/sqrt(N.gp)

      # create names
      names.gp <- rep(NA,length(mean.gp))

      for(k in 1:length(mean.gp)){
        names.gp[k] <- paste0(yvarname,"_",k)
      }
      dimnames(mean.gp) <- c(list(names.gp),list("Mean_gp"))
      dimnames(SD.gp) <- c(list(names.gp),list("SD_gp"))
      dimnames(N.gp) <- c(list(names.gp),list("Nvalid_gp"))
      dimnames(SEM.gp) <- c(list(names.gp),list("SEM_gp"))

    }

    # SPLIT OR BOTH
    if(type!="combine"){
	  numsources <- length(output)
	  mean.matrix <- NULL
	  sd.matrix <- NULL
	  n.matrix <- NULL
	  Nvalid <- 0
	  Nmissing <- 0
	  Ntotal <- 0

	  for(j in 1:numsources){
		mean.matrix <- rbind(mean.matrix,as.numeric(unlist(output[[j]][2])))
		sd.matrix <- rbind(sd.matrix,as.numeric(unlist(output[[j]][3])))
		n.matrix <- rbind(n.matrix,as.numeric(unlist(output[[j]][4])))
		Nvalid <- Nvalid+as.numeric(unlist(output[[j]][5]))
		Nmissing <- Nmissing+as.numeric(unlist(output[[j]][6]))
		Ntotal <- Ntotal+as.numeric(unlist(output[[j]][7]))
	  }
	  var.matrix <- sd.matrix^2

      mean.gp.study <- t(mean.matrix)
      SD.gp.study <- t(sd.matrix)
      N.gp.study <- t(n.matrix)
      SEM.gp.study <- SD.gp.study/sqrt(N.gp.study)

      # create names
      names.gp <- rep(NA,dim(mean.gp.study)[1])

      for(k in 1:dim(mean.gp.study)[1]){
        names.gp[k] <- paste0(yvarname,"_",k)
      }

	  names.study <- names(datasources)

      dimnames(mean.gp.study) <- c(list(names.gp),list(names.study))
      dimnames(SD.gp.study) <- c(list(names.gp),list(names.study))
      dimnames(N.gp.study) <- c(list(names.gp),list(names.study))
      dimnames(SEM.gp.study) <- c(list(names.gp),list(names.study))
    }

    if(type=="combine"){
      result <- list(mean.gp,SD.gp,N.gp,SEM.gp,Nvalid,Nmissing,Ntotal)
      names(result) <- list("Mean_gp","StDev_gp","Nvalid_gp","SEM_gp","Total_Nvalid","Total_Nmissing","Total_Ntotal")
      return(result)
    }

    if(type=="split"){
      result <- list(mean.gp.study,SD.gp.study,N.gp.study,SEM.gp.study,Nvalid,Nmissing,Ntotal)
      names(result) <- list("Mean_gp_study","StDev_gp_study","Nvalid_gp_study","SEM_gp_study","Total_Nvalid","Total_Nmissing","Total_Ntotal")
      return(result)
    }

    if(type!="combine" & type!="split"){
      mean.gp.study <- cbind(mean.gp.study,mean.gp)
      SD.gp.study <- cbind(SD.gp.study,SD.gp)
      N.gp.study <- cbind(N.gp.study,N.gp)
      SEM.gp.study <- cbind(SEM.gp.study,SEM.gp)
      dimnames(mean.gp.study) <- c(list(names.gp),list(c(names.study,"COMBINE")))
      dimnames(SD.gp.study) <- c(list(names.gp),list(c(names.study,"COMBINE")))
      dimnames(N.gp.study) <- c(list(names.gp),list(c(names.study,"COMBINE")))
      dimnames(SEM.gp.study) <- c(list(names.gp),list(c(names.study,"COMBINE")))
      result <- list(mean.gp.study,SD.gp.study,N.gp.study,SEM.gp.study,Nvalid,Nmissing,Ntotal)
      names(result) <- list("Mean_gp_study","StDev_gp_study","Nvalid_gp_study","SEM_gp_study","Total_Nvalid","Total_Nmissing","Total_Ntotal")
      return(result)
    }
  }

}
#ds.meanSdGp
