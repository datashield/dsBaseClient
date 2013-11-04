#'
#' @title  Computes the mean proportion across categories
#' @description This function calculates the mean proportion of a continuous or binary for the 
#' differerent categories of some categorical variables. The calculation can be carried out for 
#' one continuous or binary outcone variable and up to 3 catagorical explanatory variables.
#' can be processed.
#' @details The functions splits the input dataset into the single categories and fits a 
#' glm model with a constant covariate. This way, the regression coefficients from the glm fit are the 
#' log odds-ratios (LO) of the outcome variable. The LO and the standard error (se) are used to compute 
#' the lower and upper limits of the confidence interval.
#' @param datasources a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal datasources.
#' @param dataset the name given to the dataset when it was assigned from opal to R.
#' @param outvar a numeric or factor vector, the first covariate.
#' @param covar1 a numeric or factor vector, the second covariate.
#' @param covar2 a numeric or factor vector, the third covariate (optional).
#' @param covar3 a numeric or factor vector, the third covariate (optional).
#' @param CI a value between 0 and 1 that set the threshold for statisitical significance.
#' @return a list that contains the computed mean, standard error, upper and lower confidence interval, 
#' degrees of freedom of the fitted model and standard deviation.
#' @export
#' @author Gaye, A. (amadou.gaye@bristol.ac.uk); Burton, P. (p.burton@bristol.ac.uk)
#' @examples {
#' 
#' # load that contains the login details
#' data(logindata)
#' 
#' # login and assign the whole dataset on the opal server
#' opals <- datashield.login(logins=logindata,assign=TRUE)
#' 
#' # Example 1: calculate the mean proportion for LAB_HDL across gender categories
#' ds.propMean(datasources=opals, dataset=quote(D), outvar=quote(D$LAB_HDL), covar1=quote(D$GENDER))
#' 
#' # Example 2: calculate the mean proportion for LAB_HDL across gender and bmi categories
#' ds.propMean(datasources=opals, dataset=quote(D), outvar=quote(D$LAB_HDL), covar1=quote(D$GENDER), covar2=quote(D$PM_BMI_CATEGORICAL))
#' 
#' # Example 3: calculate the mean proportion for LAB_HDL across gender bmi and diabetes status categories
#' ds.propMean(datasources=opals, dataset=quote(D), outvar=quote(D$LAB_HDL), covar1=quote(D$GENDER), covar2=quote(D$PM_BMI_CATEGORICAL), covar3=quote(D$DIS_DIAB))
#' 
#' # Example 3-5: repeat the above 3 examples but for a binary outcome variable: DIS_CVA (cardio-vascular acccident)
#' ds.propMean(datasources=opals, dataset=quote(D), outvar=quote(D$DIS_CVA), covar1=quote(D$GENDER))
#' ds.propMean(datasources=opals, dataset=quote(D), outvar=quote(D$DIS_CVA), covar1=quote(D$GENDER), covar2=quote(D$PM_BMI_CATEGORICAL))
#' ds.propMean(datasources=opals, dataset=quote(D), outvar=quote(D$DIS_CVA), covar1=quote(D$GENDER), covar2=quote(D$PM_BMI_CATEGORICAL), covar3=quote(D$DIS_DIAB))
#' 
#' }
#' 
ds.propMean <-  function(datasources=NULL, dataset=NULL, outvar=NULL, covar1=NULL, covar2=NULL,  covar3=NULL, CI=0.95){

  if(is.null(datasources)){
    message("\n\n ALERT!\n")
    message(" No valid opal object(s) provided.\n")
    message(" Make sure you are logged in to valid opal server(s).\n")
    stop(" End of process!\n\n", call.=FALSE)
  }
  
  if(is.null(dataset)){
    message("\n\n ALERT!\n")
    message(" Please provide a valid dataset.\n")
    stop(" End of process!\n\n", call.=FALSE)
  }
  
  inputvars <- c()
  varnames <- c()
  
  if(is.null(outvar)){
    message("\n\n ALERT!\n")
    message(" Please provide a valid outcome variable (outvar).\n")
    stop(" End of process!\n\n", call.=FALSE)
  }else{
    inputvars <- append(inputvars, outvar)
    interm <- unlist(strsplit(deparse(outvar), "\\$", perl=TRUE))
    if(length(interm) > 1){
      assign(paste("var", 1, sep=""), strsplit(deparse(outvar), "\\$", perl=TRUE)[[1]][2])
    }else{
      assign(paste("var", 1, sep=""), strsplit(outvar, "\\$", perl=TRUE)[[1]][2])
    }
    varnames <- append(varnames, get(paste("var", 1, sep="")))
  }
  
  if(is.null(covar1)){
    message("\n\n ALERT!\n")
    message(" Please provide a valid explanatory variable (covar1).\n")
    stop(" End of process!\n\n", call.=FALSE)
  }else{
    inputvars <- append(inputvars, covar1)
    interm <- unlist(strsplit(deparse(covar1), "\\$", perl=TRUE))
     if(length(interm) > 1){
       assign(paste("var", 2, sep=""), strsplit(deparse(covar1), "\\$", perl=TRUE)[[1]][2])
     }else{
       assign(paste("var", 2, sep=""), strsplit(covar1, "\\$", perl=TRUE)[[1]][2])
     }
     varnames <- append(varnames, get(paste("var", 2, sep="")))
  }
  
  if(is.null(covar2)){
    covar2 <- NA
  }else{
    inputvars <- append(inputvars, covar2)
    interm <- unlist(strsplit(deparse(covar2), "\\$", perl=TRUE))
     if(length(interm) > 1){
       assign(paste("var", 3, sep=""), strsplit(deparse(covar2), "\\$", perl=TRUE)[[1]][2])
     }else{
       assign(paste("var", 3, sep=""), strsplit(covar2, "\\$", perl=TRUE)[[1]][2])
     }
     varnames <- append(varnames, get(paste("var", 3, sep="")))
  }
  
  if(is.null(covar3)){
    covar3 <- NA
  }else{
    inputvars <- append(inputvars, covar3)
    interm <- unlist(strsplit(deparse(covar3), "\\$", perl=TRUE))
     if(length(interm) > 1){
       assign(paste("var", 4, sep=""), strsplit(deparse(covar3), "\\$", perl=TRUE)[[1]][2])
     }else{
       assign(paste("var",4, sep=""), strsplit(covar3, "\\$", perl=TRUE)[[1]][2])
     }
     varnames <- append(varnames, get(paste("var", 4, sep="")))
  }
    
  # call the function that checks the variables are available and not empty
  vars2check <- inputvars
  datasources <- ds.checkvar(datasources, vars2check)
  
  # get the column names of the input dataset
  cols <- datashield.aggregate(datasources, paste0("colnames(",dataset, ")"))[[1]]
  
  # get the column indices of the covariates
  indx <- c()
  for(i in 2:length(varnames)){
    a <- which(cols == varnames[i])
    indx <- append(indx, a)
  }
  covarnames <- cols[indx]
  
  # carry out the first subsetting on the categories of the first covariate
  # subset the input dataframe on the categories of the 1st covariate
  D <- as.character(dataset)
  message(paste0("---Subsetting the input dataset by the variable '", covarnames[1], "'---"))
  ds.subsetdata(datasources, dataset=D, columns=list(as.numeric(indx[1])), newobj=covarnames[1])
  names.2.use.next <- covarnames[1]
  
  # if there are more than one covariate loop and subset repeatedly the dataset 
  # on the categories of each of the covariates
  if(length(indx) > 1){
    for(s in 2:length(indx)){
      # generate subsets from the datasets obtained from the previous subsetting
      # this time the subsetting is done on the next covariate
      if(s == 2){
        subsetnames <- list(datashield.aggregate(datasources, paste0("names.ds(", names.2.use.next, ")"))[[1]])
      }else{
        subsetnames <- vector("list", length(names.2.use.next))
        for(i in 1:length(names.2.use.next)){
          subsetnames[[i]] <- datashield.aggregate(datasources, paste0("names.ds(", names.2.use.next[i], ")"))[[1]]
        }
      }
      tempnames <- names.2.use.next
      cally <- paste0("levels(", paste(as.character(dataset),"$", covarnames[s-1], sep=""), ")")
      categories <- datashield.aggregate(datasources, cally)[[1]]
      names.2.use.next <- c()
      message(paste0("\n---Subsetting the ", covarnames[s-1], " subsetted datasets by the variable '", covarnames[s]), "'---")
      for(i in 1:length(subsetnames)){
        # here is where the subsetting is carried out, using the output of previous subsetting
        for(j in 1:length(categories)){
          # name of the dataset to subset
          dt2subset <- paste0(tempnames[i],"$",subsetnames[[i]][j])
          # column/variable by which to subset 
          subsetby <- list(as.numeric(indx[s]))
          # name of the list object to hold the subset datasets
          outlist <- paste0(covarnames[s],".",covarnames[s-1],"_",categories[j])
          ds.subsetdata(datasources, dataset=dt2subset, columns=subsetby, newobj=outlist)
          names.2.use.next <- append(names.2.use.next, outlist)
        }
      }
      # this vector with hold the names of all the subsets (each names point a list which contains the subsetted datasets)
      # THESE LISTS HOLD THE DATASETS FROM THE LAST SUBSETTING PROCESS
      if(s == length(indx)) { all.subsets.lists <- names.2.use.next }
     }
  }else{
    # if there is only one covariate e.g. mean LAB_HDL by gender only
    # there must be at least one covariate otherwise the process is stopped and an alert issued
    # the subsetting is done already, so here we only get the name of the list that holds the subsets
    all.subsets.lists <- names.2.use.next
  }
  
  # print the names of the lists that contains the subset datasets and the names of the subset datasets
  message("\nNames of the list objects, on the server side that hold the final subset datasets:")
  print(all.subsets.lists)
  
  # THE SUBSETTING DOES NOT RETURN A DATASET IF THE DATASET TO RETUN IS NOT VALID; SO HERE WE 
  # MAKE SURE THAT WE CARRY ONT ONLY WITH THE SUBSETS THAT WERE GENERATED (I.E. THE VALID ONES)
  
  # get the column of the outcome variable from the hell of subsets dataset in the hell of lits
  # loop trough each list and grab the outcome from each subset dataset
  record.invalids <- c()
  for(i in 1: length(all.subsets.lists)){
    # check for invlaid subsets
    valcheck <- datashield.aggregate(datasources, paste0("is.null(",all.subsets.lists[i],")"))
    if(length(which(valcheck == TRUE)) > 0){
      record.invalids <- append(record.invalids, as.numeric(as.character(which(valcheck == TRUE))))
    }
  }
  
  if(length(unique(as.numeric(as.character(record.invalids)))) > 0){
    if(length(unique(as.numeric(as.character(record.invalids)))) >= length(datasources)){
      stop("---SOME SUBSET DATASETS WERE NOT VALID IN ANY STUDIES; THE CALCULATIONS CANNOT BE CARRIED OUT.---")
    }else{
      study2rm <- unique(as.numeric(as.character(record.invalids)))
      message("SOME SUBSET DATASETS WERE NOT VALID IN STUDIES ", paste0(datasources[-study2rm], collapse=","),"!")
      message("THESE STUDIES WILL NOT BE INCLUDED IN THE CALCULATIONS.")
      warning("SOME SUBSET DATASETS WERE NOT VALID IN STUDIES ", paste0(datasources[-study2rm], collapse=","),";THESE STUDIES WERE NOT BE INCLUDED IN THE CALCULATIONS.")
      datasources <- datasources[-study2rm]
    }
  }
  
  message(paste0("\n---Running GLM, with Grand mean, for the outcome, in each of the subsets---"))
  
  # get the outcome vectors to run glm for
  outvectors <- c()
  for(i in 1: length(all.subsets.lists)){
    n <- datashield.aggregate(datasources, paste0("names.ds(",all.subsets.lists[i],")"))[[1]]
    l <- length(n)
    for(j in 1: l){
      outvectors <- append(outvectors, paste0(all.subsets.lists[i], "$", n[j], "$", var1))
    }
  }

  # set the link function/family function correctly
  # if the outcome variable is not a numeric or a factor, issue a message and stop the process
  if(ds.is.factor(datasources, outvar)[[1]]){ 
    fam <- quote(binomial)
  }else{
    if(ds.is.numeric(datasources, outvar)[[1]]){ 
      fam <- quote(gaussian)
    }else{
      stop("The outcome variable must be a numeric or a factor!")
    }
  }
  
  # run the glm for each of the outcome variable in each of the subsets
  glmout <- list()
  for(i in 1:length(outvectors)){
    form <- as.formula(paste0(outvectors[i],"~",1)) 
    # the glm function, this is 'ds.glm' without its header checks which crash due to the long 
    # name of the outcome variable resulting from several subsetting
    glmout[[i]] <- dsbaseclient:::propMeanHelper(datasources,formula=form, family=fam)
    message("Summary GLM fit for  ", outvectors[i], "\n")
    print(glmout[[i]])
    message("\n")
  }
  
  # calculate mean, se, lCI, uCI, deviance etc...
  # store these values together for each fit
  percent.CI <- CI
  scalar <- qnorm(1-(1-percent.CI)/2)
  
  finaloutput <- list()
  for(i in 1:length(glmout)){
    
    mean.m <- glmout[[i]]$coefficients[1]
    se.m <- glmout[[i]]$coefficients[2]
    l95.m <- mean.m-scalar*se.m
    u95.m <- mean.m+scalar*se.m
    N.m <- glmout[[i]]$df
    stdev.m <- se.m*sqrt(N.m)
    finaloutput[[i]] <- round(c(mean.m, se.m, l95.m, u95.m, N.m, stdev.m),4)
    
  }
  names(finaloutput) <- paste0(outvectors, ": mean, se, l95, u95, N and stdev")
  
  return(finaloutput)
}
