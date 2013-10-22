
ds.stratmean  <-  function(datasources=NULL, datasetname=NULL, outvect=NULL, covar1=NULL, covar2=NULL, CI=0.95){

  if(is.null(datasources)){
    cat("\n\n ALERT!\n")
    cat(" No valid opal object(s) provided.\n")
    cat(" Make sure you are logged in to valid opal server(s).\n")
    stop(" End of process!\n\n", call.=FALSE)
  }
  
  if(is.null(datasetname)){
    cat("\n\n ALERT!\n")
    cat(" Please provide a valid datasetname.\n")
    stop(" End of process!\n\n", call.=FALSE)
  }
  
  
  if(is.null(outvect)){
    cat("\n\n ALERT!\n")
    cat(" Please provide a valid outcome variable (outvect).\n")
    stop(" End of process!\n\n", call.=FALSE)
  }
  
  if(is.null(covar1)){
    cat("\n\n ALERT!\n")
    cat(" Please provide a valid explanatory variable (covar1).\n")
    stop(" End of process!\n\n", call.=FALSE)
  }

  if(is.null(covar2)){
    cat("\n\n ALERT!\n")
    cat(" Please provide a valid explanatory variable (covar2).\n")
    stop(" End of process!\n\n", call.=FALSE)
  }
  
  # get the name of the input variables
  # the input variables might be given as column table (i.e. D$xvect)
  # or just as a vector not attached to a table (i.e. xvect)
  # we have to make sure the function deals with each case
  interm1 <- unlist(strsplit(deparse(outvect), "\\$", perl=TRUE))
  if(length(interm1) > 1){
    var1 <- strsplit(deparse(outvect), "\\$", perl=TRUE)[[1]][2]
  }else{
    var1 <- deparse(outvect)
  }
  
  interm2 <- unlist(strsplit(deparse(covar1), "\\$", perl=TRUE))
  if(length(interm2) > 1){
    var2 <- strsplit(deparse(covar1), "\\$", perl=TRUE)[[1]][2]
  }else{
    var2 <- deparse(covar1)
  }
  
  interm3 <- unlist(strsplit(deparse(covar2), "\\$", perl=TRUE))
  if(length(interm3) > 1){
    var3 <- strsplit(deparse(covar2), "\\$", perl=TRUE)[[1]][2]
  }else{
    var3 <- deparse(covar2)
  }

  # call the function that checks the variables are available and not empty
  vars2check <- list(outvect, covar1, covar2)
  datasources <- ds.checkvar(datasources, vars2check)
  
  # get the column names of the input dataset
  cols <- datashield.aggregate(datasources, paste0("colnames(",datasetname, ")"))[[1]]
  
  # get the column indices of the two covariates
  indx <- c()
  invar <- c(var2, var3)
  for(i in 1:2){
    a <- which(cols == invar[i])
    indx <- append(indx, a)
  }
  
  # create subsets of the input dataframe based on the levels of two covariates
  cally <- call("subsetdata.ds", paste0("",datasetname,""), as.list(indx))
  datashield.assign(datasources, "SS", cally)

  # display the names of the subset datasets in each study
  cat("\nNames of the subset datasets in each study:\n\n")
  ssnames <- datashield.aggregate(opals, quote(names.ds(SS)))
  print(datashield.aggregate(opals, quote(names.ds(SS))))

  # display the length of the 1st covariate in the datatset subset by that covariate
  levels1 <- datashield.aggregate(opals, paste0("levels(", paste(datasetname,"$", var2, sep=""), ")"))[[1]]
  cat("\nLength of the variable ", var2, " in the datasets subsetted by ", var, " categories:\n")
  for(i in 1:length(levels1)){
    lev <- as.numeric(levels1[i])
    cat(paste(ssnames[[1]][lev], "-", var2, "\n"))
    print(datashield.aggregate(opals, paste0("length(", paste("SS$", ssnames[[1]][lev], "$", var2, sep=""), ")")))
  }

  # genereate subsets from the dataset subsetted on the 1st covariate already
  a.names <- c()
  for(i in 1:length(levels1)){
    lev <- as.numeric(levels1[i])
    a.name <- paste("SS", lev, sep="")
    a.names <- append(a.names, a.name)
    datashield.assign(opals, a.name, paste0("subsetdata.ds(", paste("SS$", ssnames[[1]], sep=""), ")"))
  }

  # give the covar2 subsetted datasets  new names
  levels2 <- datashield.aggregate(opals, paste0("levels(", paste(datasetname,"$", var3, sep=""), ")"))[[1]]
  b.names <- c()
  for(i in 1:length(levels1)){
    for(j in 1:length(levels2)){
      lev1 <- as.numeric(levels1[i])
      lev2 <- as.numeric(levels2[j])
      a.name <- a.names[i]
      b.name <- paste("qvar.", lev1, ".", lev2, sep="")
      b.names <- append(b.names, b.name)
      datashield.assign(opals, b.name, paste0("(", paste(a.name, "$", ssnames[[2]], "$", var3, sep=""), ")"))
    }
  }
  
  # now run the glm, setting the linear predictor as a constant; keep results in a list
  glmres <- list()
  count <- 0
  for(i in 1:length(levels1)){
    for(j in 1:length(levels2)){
      count <- count+1
      glmres[[count]] <- ds.glm(datasources,formula=b.names[count]~1, family="gaussian")
      cat("Summary GLM fit for  ", b.names[count], "\n")
      print(glmres[[count]])
      cat("\n\n")
    }
  }
    
  mod.m.1
  mod.m.2
  mod.m.3
  mod.m.4
  mod.m.5

  mod.f.1
  mod.f.2
  mod.f.3
  mod.f.4
  mod.f.5

  # calculate mean, se, lCI, uCI, deviance etc...
  # store these values together for each fit
  percent.CI <- CI
  scalar <- qnorm(1-(1-percent.CI)/2)

  final.res <- list()
  for(i in 1:length(glm.res)){
  
    mean.m <- glmres[[i]]$coefficients[1]
    se.m <- glmres[[i]]$coefficients[2]
    l95.m <- mean.m-scalar*se.m
    u95.m <- mean.m+scalar*se.m
    N.m <- glmres[[i]]$df
    stdev.m <- se.m*sqrt(N.m)
    final.res[[i]] <- c(mean.m, se.m, l95.m, u95.m, N.m, stdev.m)
  }
  names(final.res) <- b.names

  return(final.res)
}
