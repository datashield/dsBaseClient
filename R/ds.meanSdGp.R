#'
#' @title  Computes the mean and standard deviation across groups defined by one factor
#' @description This function calculates the mean and SD of a continuous variable for each class of
#' a single factor.
#' @details This function calculates the mean, standard deviation (SD), N (number of observations)
#' and the standard error of the mean (SEM) of a continuous variable broken down into subgroups
#' defined by a single factor. It also reports the total number of observations (=Ntotal), the
#' total number of valid observations, i.e. non-missing observations = observations where neither
#' the continuous variable nor the factor is misssing, (Nvalid) and the total number of missing
#' observations (Nmissing). Nvalid=Ntotal-Nmissing and all three quantities represent the sum
#' across all groups and all studies. If any one subgroup consists of between 1 and "nfilter"
#' observations, the function simply reports that fact and suggests that you use a different
#' grouping variable. As in other functions such as ds.table1D, the value of nfilter can be chosen
#' by the data custodian when each Opal server is originally set up. By default it is set to 5.
#' There are IMPORTANT DIFFERENCES between ds.meanSdGp compared to the function ds.meanByClass. 
#'(A) ds.meanSdGp does not actually subset the data it simply calculates the required statistics
#' and reports them. This means you cannot use this function if you wish to physically break the
#' data into subsets. On the other hand, it makes the function very much faster than ds.meanByClass
#' if you do not need to create physical subsets. (B) ds.meanByClass allows you to specify up to
#' three categorising factors, but ds.meanSdGp only allows one. However, this is not a serious
#' problem. If you have two factors (e.g. sex with two levels [0 and 1] and BMI.categorical with
#' three levels [1,2,3]) you simply need to create a new factor that combines the two together in a
#' way that gives each combination of levels a different value in the new factor. So, in the
#' example given, the calculation newfactor=(3*sex)+BMI gives you six values: sex=0, BMI=1, 
#' newfactor=1; sex=0, BMI=2, newfactor=2; sex=0, BMI=3, newfactor=3; sex=1, BMI=1, newfactor=4;
#  sex=1, BMI=2, newfactor=5; sex=1, BMI=3, newfactor=6. This calculation can be done with a single
#' ds.assign command and you then use newfactor as the single categorising factor. (C) At present,
#' ds.meanByClass calculates the sample size in each group to mean the TOTAL sample size (i.e. it
#' includes all observations in each group regardless whether or not they include missing values
#' for the continuous variable or the factor). The calculation of sample size in each group by
#' ds.meanSdGp always reports the number of observations that are non-missing both for the
#' continuous variable and the factor. This makes sense - in the case of ds.meanByClass, the total
#' size of the physical subsets was important, but when it comes down only to ds.meanSdGp which
#' undertakes analysis without physical subsetting,  it is only the observations with non-missing
#' values in both variables that contribute to the calculation of means and SDs within each group
#' and so it is logical to consider those counts as primary. The only reference ds.meanSdGp makes
#' to missing counts is in the reporting of Ntotal and Nmissing overall (ie not broken down by
#' group). For the future, we plan to extend ds.meanByClass to report both total and non-missing
#' counts in subgroups. 
#' @param x This must be named as a character string (e.g. "AGE"). It must denote a continuous
#' variable of class numeric.
#' @param y This must be named as a character string (e.g. "sex"). It must denote a categorical
#' variable of class factor.
#' @param type This must be specified as a character string ("combine", "split" or "both"). If
#' "combine" the results for each group are reported combined over all studies. If "split" the
#' table of means by group (for example) has a separate column for each study. If "both" the tables
#' each have an additional column reporting the sum across all studies in each group in addition to
#' the columns (produced by "split") for each study alone. This parameter defaults to "both" unless
#' "combine" or "split" are specified.
#' @param do.checks This parameter defaults to FALSE. It determines whether administrative checks
#' are undertaken to ensure that the input objects are defined in all studies, and that the
#' variables are of equivalent class in each study. By defaulting the checks to FALSE, we save
#' time, and if you hit a problem that you cannot understand you can reset do.checks to TRUE and
#' make sure that the input objects are correctly defined in all studies.
#' @param datasources a list of opal object(s) obtained after login in to opal servers; these
#' objects hold also the data assign to R, as \code{dataframe}, from opal datasources. If no
#' datasources are specified, DataSHIELD looks for opal objects. But, beware, if you have several
#' different sets of opal objects in your analysis space, if you do not explicitly specify which
#' ones you want, you may be asked to state which ones you want after every command or you may get
#' a repeated warning about it after every command. In general, you will typically only need one
#' set of opal objects in any one setting and so this problem will not arise. The datasources
#' argument also allows you to restrict analysis to one or more specific studies rather than all
#' studies. For example, if the Opal objects are called Opal.servers and there are five of them,
#' datasources=Opal.servers[3], will restrict the analysis to the server listed third in your list
#' of servers.
#' @return If type = "combine" the function returns a list consisting of a four tables denoting:
#' mean by group; standard deviation (SD) by group; number of non-missing observations (Nvalid) by
#' group; and standard error of the mean (SEM) by group. All of these are COMBINED ACROSS STUDIES.
#' For information, SEM = SD/sqrt(Nvalid). These are all returned in list format with names:
#' Mean_gp, StDev_gp, Nvalid_gp and SEM_gp. If you need to use them in their original class (e.g.
#' matrix), you need to use the conventional R function unlist() to convert them back to their
#' original form. The output list also includes: Total_Nvalid (the total number of valid
#' [non-missing] observations across all groups in all studies; Total_Nmissing (the total number of
#' observations with either or both x and y missing); and Total_Ntotal (the total number of
#' observations [with data missing or not]). If type ="split", the mean, SD, Nvalid and SEM are
#' reported by group and by study. The first four elements of the returned output list are 
#' therefore: Mean_gp_study; StDev_gp_study; Nvalid_gp_study; and SEM_gp_study. If there are three
#' studies and we are breaking things down by five groups in each study, each of the first four
#' list elements consists of a table with five rows (one for each group) and three columns (one for
#' each study). The returned output also includes Total_Nvalid, Total_Nmissing and Total_Ntotal as
#' before. If type = "both" the output is precisely the same as with type="split" and each of its
#' components has the same name, but each table (e.g. Mean_gp_study) will now have an extra column
#' on the right had side (so a fourth column in the example above) which contains the appropriate
#' combined value in each group across all studies together. In other words, the four columns
#' replicate the results obtained when type = "combine". CRUCIALLY, IF ONE OR MORE OF THE GROUPS IN
#' ANY OF THE STUDIES CONTAINS BETWEEN 1 and nfilter OBSERVATIONS, the returned output list will
#' ONLY include the warning: [1] "At least 1 cell count is 1-nfilter, please regroup".
#' @author Burton PR
#' @seealso \link{ds.subsetByClass} to subset by the classes of factor vector(s).
#' @seealso \link{ds.subset} to subset by complete cases (i.e. removing missing values), threshold,
#' columns and rows.
#' @export
#' @examples
#' \dontrun{
#' 
#' # #load that contains the login details
#' # data(logindata)
#' 
#' # #Example 1: Calculate the mean, SD, Nvalid and SEM of the continuous variable AGE.60 (age in
#' # #years centralised at 60), broken down by TID.f (a six level factor relating to survival time)
#' # #and report the pooled results combined across studies.
#' # ds.meanSdGp("AGE.60","TID.f","combine")
#' 
#' # #Example 2: Calculate the mean, SD, Nvalid and SEM of the continuous variable AGE.60 (age in
#' # #years centralised at 60), broken down by TID.f (a six level factor relating to survival time)
#' # #and report both study-specific results and the pooled results combined across studies. Do the
#' # #checks for consistency of variables in all studies. Save the returned output to msg.b.
#' # msg.b <- ds.meanSdGp("AGE.60", "SEXF", "both", do.checks=TRUE)
#' # msg.b
#' # PRODUCES THIS OUTPUT
#' # $Mean_gp_study
#' #           study1    study2   COMBINE
#' # SEXF_1 -4.099893 -5.199134 -4.568966
#' # SEXF_2 -2.384477 -3.057421 -2.690300
#' #
#' # $StDev_gp_study
#' #          study1   study2  COMBINE
#' # SEXF_1 13.67313 14.52537 14.04313
#' # SEXF_2 14.87182 14.64741 14.77026
#' #
#' # $Nvalid_gp_study
#' #        study1 study2 COMBINE
#' # SEXF_1    931    693    1624
#' # SEXF_2   1108    923    2031
#' #
#' # $SEM_gp_study
#' #           study1    study2   COMBINE
#' # SEXF_1 0.4481188 0.5517731 0.3484744
#' # SEXF_2 0.4467804 0.4821253 0.3277427
#' #
#' # $Total_Nvalid
#' # [1] 3655
#' #
#' # $Total_Nmissing
#' # [1] 45
#' #
#' # $Total_Ntotal
#' # [1] 3700
#' #
#' # Example 3: 
#' # Calculate the mean, SD, Nvalid and SEM of the continuous variable SBP (systolic BP), broken
#' # down by CVA (1 = had a stroke, 0 = no stroke) report the study-specific results only. The
#' # output shows that there are inadequate numbers of stroke cases to carry out this particular
#' # analysis: at least one cell contains between 1 and nfilter (the chosen value of the disclosure
#' # filter - typically 4) observations. Given that the CVA grouping is as simple as it can be, it
#' # is impossible to regroup in this setting. The only option would be to have chosen a different
#' # level for the nfilter. If it is 0, there is no limitation on cell counts: but whether or not
#' # cell counts in the range, say, 1-4 are to be viewed as providing a significant risk is
#' # something that should be decided before the analysis starts. In reality, for many biomedical
#' # studies, particularly when data users have signed a data access agreement explicitly stating
#' # they will not try to identify individuals or infer their characteristics, many researchers may
#' # choose to turn the disclosure filter off (nfilter=0). But for particularly sensitive data, or
#' # data obtained from official governmental sources, e.g. census data, there may simply be no
#' # option but to pick a filter of say 4. 
#' # ds.meanSdGp("SBP", "CVA", "split")
#' # PRODUCES THIS OUTPUT
#' # [1] "At least 1 cell count is 1-nfilter, please regroup"
#' #
#' # clear the Datashield R sessions and logout
#' #datashield.logout(opals)
#' 
#' }
#'
ds.meanSdGp <- function(x=NULL, y=NULL, type='both', do.checks=FALSE, datasources=NULL){ 
  
  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }
  
  if(is.null(x)){
    stop("Please provide the name of the input vector!", call.=FALSE)
  }

  if(is.null(y)){
    stop("Please provide the name of the input vector!", call.=FALSE)
  }
  
  # the input variable might be given as column table (i.e. D$x)
  # or just as a vector not attached to a table (i.e. x)
  # we have to make sure the function deals with each case
  xnames <- extract(x)
  ynames <- extract(y)
  xvarname <- xnames$elements
  yvarname <- ynames$elements
  xobj2lookfor <- xnames$holders
  yobj2lookfor <- ynames$holders
  xvariable <- xvarname
  yvariable <- yvarname
 
  if(do.checks)
  { 
    # check if the input object(s) is(are) defined in all the studies
    if(is.na(xobj2lookfor)){
      defined <- isDefined(datasources, xvarname)
    }else{
      defined <- isDefined(datasources, xobj2lookfor)
    }
    if(is.na(yobj2lookfor)){
      defined <- isDefined(datasources, yvarname)
    }else{
      defined <- isDefined(datasources, yobj2lookfor)
    }
    # call the internal function that checks the input object is of the same class in all studies.
    typ1 <- checkClass(datasources, x)
    typ2 <- checkClass(datasources, y)
  }
 
  # names of the studies 
  stdnames <- names(datasources)
  
  # call the server side function that calculates mean and DS by group in each study

  calltext <- paste0("meanSdGpDS(", x, ",", y, ")")
  output <- opal::datashield.aggregate(datasources, as.symbol(calltext))

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
