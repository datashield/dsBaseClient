#' @title Fits Generalized Linear Model via Study-Level Meta-Analysis
#' @description Fits a generalized linear model (GLM) on data from single or multiple sources
#' with pooled co-analysis across studies being based on SLMA (Study-Level Meta-Analysis). 
#' @details \code{ds.glmSLMA} specifies the structure of a Generalized Linear Model 
#' to be fitted separately on each study or data source. 
#' From a mathematical perspective, the SLMA approach (using \code{ds.glmSLMA})
#' differs fundamentally from the usual approach using \code{ds.glm}
#' in that the latter is mathematically equivalent
#' to placing all individual-level data from all sources in
#' one central warehouse and analysing those data as one combined dataset using the
#' conventional \code{glm()} function in R. 
#' 
#' However, although this
#' may sound to be preferable under all circumstances, the SLMA approach
#' offers key inferential advantages when there is marked heterogeneity
#' between sources that cannot simply be corrected with fixed-effects each reflecting a study
#' or centre-effect. In particular, fixed effects cannot simply be used in this way when 
#' there is heterogeneity in the effect that is of scientific interest.
#' 
#' In \code{formula} Most shortcut notation for formulas allowed under R's standard \code{glm()}
#' function is also allowed by \code{ds.glmSLMA}. 
#' 
#' Many glms can be fitted very simply using a formula such as:
#' 
#' \deqn{y~a+b+c+d} 
#' 
#' which simply means fit a glm with \code{y} as the outcome variable and 
#' \code{a}, \code{b}, \code{c} and \code{d} as covariates. 
#' By default all such models also include an intercept (regression constant) term.
#' 
#' Instead, if you need to fit a more complex
#' model, for example:
#' 
#'  \deqn{EVENT~1+TID+SEXF*AGE.60}
#'
#' In the above model the outcome variable is \code{EVENT} 
#' and the  covariates 
#' \code{TID} (factor variable with level values between 1 and 6 denoting the period time), 
#' \code{SEXF} (factor variable denoting sex)
#' and \code{AGE.60} (quantitative variable representing age-60 in years). 
#' The term \code{1} forces
#' the model to include an intercept term, in contrast if you use the term \code{0} the 
#' intercept term is removed. The \code{*} symbol  between \code{SEXF} and \code{AGE.60}
#' means fit all possible main effects and interactions for and between those two covariates.
#'  This takes the value 0 in all males \code{0 * AGE.60} 
#'  and in females  \code{1 * AGE.60}. 
#'  This model is in example 1 of  the section \strong{Examples}. In this case the logarithm of 
#'  the survival time is added as an offset (\code{log(survtime)}).
#' 
#' In the \code{family} argument can be specified three types of models to fit:
#' 
#'  \itemize{
#'    \item{\code{"gaussian"}}{: conventional linear model with normally distributed errors} 
#'    \item{\code{"binomial"}}{: conventional unconditional logistic regression model}
#'    \item{\code{"poisson"}}{: Poisson regression model which is the most used in survival analysis. 
#'     The model used Piecewise Exponential Regression (PER) which typically closely approximates
#'     Cox regression in its main estimates and standard errors.}
#' }
#' 
#' 
#' At present the gaussian family is automatically coupled with
#' an \code{identity} link function, the binomial family with a
#' \code{logistic} link function and the poisson family with a \code{log} link function. 
#' 
#' However, if a particular user
#' wishes us to implement an alternative family
#' (e.g. \code{gamma}) or an alternative family/link combination (e.g. binomial with
#' probit) we can discuss how best to meet that request: it will almost certainly be possible,
#' but we may seek a small amount of funding or practical in-kind support from
#' the user to ensure that it can be carried out promptly.
#' 
#'  The \code{dataName} argument avoids you having to specify the name of the
#' data frame in front of each covariate in the formula. 
#' For example, if the data frame is called \code{DataFrame} you
#' avoid having to write: \eqn{DataFrame$y~DataFrame$a+DataFrame$b+DataFrame$c+DataFrame$d}
#' 
#' The \code{checks} argument verifies that the variables in the model are all defined (exist) 
#' on the server-site at every study
#' and that they have the correct characteristics required to fit the model. 
#' It is suggested to make \code{checks} argument TRUE if an unexplained
#'  problem in the model fit is encountered because the running process takes several minutes.
#'  
#' In \code{maxit} Logistic regression and Poisson regression
#' models can require many iterations, particularly if the starting value of the
#' regression constant is far away from its actual value that the GLM
#' is trying to estimate. In consequence we often set \code{maxit=30}
#' but depending on the nature of the models you wish to fit, you may wish
#' to be alerted much more quickly than this if there is a delay in convergence, 
#' or you may wish to all more iterations.
#' 
#' 
#' Server functions called: \code{glmSLMADS1} and \code{glmSLMADS2}
#' @param formula an object of class formula describing
#' the model to be fitted. For more information see 
#' \strong{Details}.  
#' @param family identifies the error distribution function to use in
#' the model. 
#' @param offset  a character string specifying the name of a variable to be used as
#' an offset.\code{ds.glmSLMA} does not allow an offset vector to be
#' written directly into the GLM formula.  
#' @param weights a character string specifying the name of a variable containing
#' prior regression
#' weights for the fitting process. \code{ds.glmSLMA} does not allow a weights vector to be
#' written directly into the GLM formula.
#' @param dataName a character string specifying the name of an (optional) data frame that contains
#' all of the variables in the GLM formula. 
#' @param checks logical. If TRUE \code{ds.glmSLMA} checks the structural integrity 
#' of the model. Default FALSE. For more information see \strong{Details}.
#' @param maxit a numeric scalar denoting the maximum number of iterations that
#' are permitted before \code{ds.glmSLMA} declares that the model has failed to converge. 
#' For more information see \strong{Details}.
#' @param combine.with.metafor logical. If TRUE the
#' estimates and standard errors for each regression coefficient are pooled across
#' studies using random-effects meta-analysis under maximum likelihood (ML),
#' restricted maximum likelihood (REML) or fixed-effects meta-analysis (FE). Default TRUE. 
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login. 
#' If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link{datashield.connections_default}}.
#' @return Many of the elements of the output list returned by \code{ds.glmSLMA} are 
#' equivalent to those returned by the \code{glm()} function in native R. However,
#' potentially disclosive elements
#' such as individual-level residuals and linear predictor values are blocked. 
#' In this case, only non-disclosive elements are returned from each study separately.
#' 
#' The list of elements returned by \code{ds.glmSLMA} is mentioned below: 
#' 
#' @return \code{coefficients}: a matrix with 5 columns:
#'    \itemize{
#'    \item{First}{: the names of all of the regression parameters (coefficients) in the model} 
#'    \item{second}{: the estimated values} 
#'    \item{third}{: corresponding standard errors of the estimated values} 
#'    \item{fourth}{: the ratio of estimate/standard error} 
#'    \item{fifth}{: the p-value treating that as a standardised normal deviate} 
#' }
#' @return \code{family}: indicates the error distribution and link function used
#' in  GLM.
#' @return \code{formula}: model formula, see description of formula as an input parameter (above).
#' @return \code{df.resid}: the residual degrees of freedom around the model.
#' @return \code{deviance.resid}: the residual deviance around the model.
#' @return \code{df.null}: the degrees of freedom around the null model (with just an intercept).
#' @return \code{dev.null}: the deviance around the null model (with just an intercept).
#' @return \code{CorrMatrix}: the correlation matrix of parameter estimates.
#' @return \code{VarCovMatrix}: the variance-covariance matrix of parameter estimates.
#' @return \code{weights}: the vector (if any) holding regression weights.
#' @return \code{offset}: the vector (if any) holding an offset (enters glm with a
#' coefficient of 1.00).
#' @return \code{cov.scaled}: equivalent to \code{VarCovMatrix}.
#' @return \code{cov.unscaled}: equivalent to VarCovMatrix but assuming dispersion (scale)
#' parameter is 1.
#' @return \code{Nmissing}: the number of missing observations in the given study.
#' @return \code{Nvalid}: the number of valid (non-missing) observations in the given study.
#' @return \code{Ntotal}: the total number of observations in the given study 
#'                        (\code{Nvalid} + \code{Nmissing}).
#' @return \code{data}: equivalent to input parameter \code{dataName} (above).
#' @return \code{dispersion}: the estimated dispersion parameter: deviance.resid/df.resid for
#' a gaussian family multiple regression model, 1.00 for logistic and poisson regression.
#' @return \code{call}:  summary of key elements of the call to fit the model.
#' @return \code{na.action}:  chosen method of dealing with missing values.
#'  Usually, \code{na.action = na.omit}
#' indicating any individual (or more strictly any "observational unit")
#' that has any data missing that are needed for the model is
#' excluded from the fit, even if all the rest of the required data are present.
#' These required data include: the outcome variable, covariates,
#' or any values in a regression weight vector or offset vector. As a
#' side effect of this, when you include additional covariates in the model
#' you may exclude extra individuals from the analysis
#' and this can seriously distort inferential tests based on assuming models are
#' nested (eg likelihood ratio tests).
#' @return \code{iter}: the number of iterations required to achieve convergence
#' file for the \code{glm()} function in native R.
#' @return Once the study-specific output has been returned, the function returns the
#' number of elements relating to the pooling of estimates across studies via
#' study-level meta-analysis. These are as follows:
#' @return \code{input.beta.matrix.for.SLMA}: a matrix containing the vector of coefficient
#' estimates from each study.
#' @return \code{input.se.matrix.for.SLMA}: a matrix containing the vector of standard error
#' estimates for coefficients from each study.
#' @return \code{SLMA.pooled.estimates}: a matrix containing pooled estimates for each
#' regression coefficient across all studies with pooling under SLMA via
#' random-effects meta-analysis under maximum likelihood (ML), restricted maximum
#' likelihood (REML) or via fixed-effects meta-analysis (FE).
#' @return \code{convergence.error.message}:  reports for each study whether the model converged.
#' If it did not some information about the reason for this is reported.
#' @author DataSHIELD Development Team
#' @examples
#' \dontrun{
#' 
#'  ## Version 6, for version 5 see Wiki
#'   # Connecting to the Opal servers
#'   
#'   require('DSI')
#'   require('DSOpal')
#'   require('dsBaseClient')
#'   
#'   # Example 1: Fitting GLM for survival analysis
#'   # For this analysis we need to load survival data from the server 
#'   
#'   builder <- DSI::newDSLoginBuilder()
#'   builder$append(server = "study1", 
#'                  url = "http://192.168.56.100:8080/", 
#'                  user = "administrator", password = "datashield_test&", 
#'                  table = "SURVIVAL.EXPAND_NO_MISSING1", driver = "OpalDriver")
#'   builder$append(server = "study2", 
#'                  url = "http://192.168.56.100:8080/", 
#'                  user = "administrator", password = "datashield_test&", 
#'                  table = "SURVIVAL.EXPAND_NO_MISSING2", driver = "OpalDriver")
#'   builder$append(server = "study3",
#'                  url = "http://192.168.56.100:8080/", 
#'                  user = "administrator", password = "datashield_test&", 
#'                  table = "SURVIVAL.EXPAND_NO_MISSING3", driver = "OpalDriver")
#'   logindata <- builder$build()
#'   
#'   # Log onto the remote Opal training servers
#'   connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D") 
#'   
#'   # Fit the GLM 
#'   
#'   # make sure that the outcome is numeric 
#'   ds.asNumeric(x.name = "D$cens",
#'                newobj = "EVENT",
#'                datasources = connections)
#'                
#'   # convert time id variable to a factor 
#'                
#'   ds.asFactor(input.var.name = "D$time.id",
#'               newobj = "TID",
#'               datasources = connections)
#'               
#'   # create in the server-side the log(survtime) variable
#'          
#'   ds.log(x = "D$survtime",
#'          newobj = "log.surv",
#'          datasources = connections)
#'   
#'   ds.glmSLMA(formula = EVENT ~ 1 + TID + female * age.60,
#'          data = "D",
#'          family = "poisson", 
#'          offset = "log.surv",
#'          weights = NULL,
#'          checks = FALSE,
#'          maxit = 20,
#'          datasources = connections)
#'          
#'   # Clear the Datashield R sessions and logout
#'   datashield.logout(connections) 
#'   
#'   # Example 2: run a logistic regression without interaction
#'   # For this example we are going to load another type of data  
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
#'   # Fit the logistic regression model
#' 
#'   mod <- ds.glmSLMA(formula = "DIS_DIAB~GENDER+PM_BMI_CONTINUOUS+LAB_HDL",
#'                 data = "D",
#'                 family = "binomial",
#'                 datasources = connections)
#'                 
#'   mod #visualize the results of the model
#' 
#' # Example 3: fit a standard Gaussian linear model with an interaction
#' # We are using the same data as in example 2. It is not necessary to
#' # connect again to the server 
#' 
#' mod <- ds.glmSLMA(formula = "PM_BMI_CONTINUOUS~DIS_DIAB*GENDER+LAB_HDL",
#'               data = "D",
#'               family = "gaussian",
#'               datasources = connections)
#' mod
#' 
#' # Clear the Datashield R sessions and logout
#' datashield.logout(connections) 
#' }
#'
#' @export
ds.glmSLMA<-function(formula=NULL, family=NULL, offset=NULL, weights=NULL, combine.with.metafor=TRUE,dataName=NULL,
checks=FALSE, maxit=30, datasources=NULL) {

  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }



  # verify that 'formula' was set
  if(is.null(formula)){
    stop(" Please provide a valid regression formula!", call.=FALSE)
  }

# check if user gave offset or weights directly in formula, if so the argument 'offset' or 'weights'
# to provide name of offset or weights variable
    if(sum(as.numeric(grepl('offset', formula, ignore.case=TRUE)))>0 ||
       sum(as.numeric(grepl('weights', formula, ignore.case=TRUE)))>0)
{
       cat("\n\n WARNING: you may have specified an offset or regression weights")
       cat("\n as part of the model formula. In ds.glm (unlike the usual glm in R)")
       cat("\n you must specify an offset or weights separately from the formula")
       cat("\n using the offset or weights argument.\n\n")
}

  formula <- stats::as.formula(formula)


  # check that 'family' was set
  if(is.null(family)){
    stop(" Please provide a valid 'family' argument!", call.=FALSE)
  }

  # if the argument 'dataName' is set, check that the data frame is defined (i.e. exists) on the server site
  if(!(is.null(dataName))){
    defined <- isDefined(datasources, dataName)
  }

  # beginning of optional checks - the process stops if any of these checks fails #
  if(checks){
    message(" -- Verifying the variables in the model")
    # call the function that checks the variables in the formula are defined (exist) on the server site and are not missing at complete

   glmChecks(formula, dataName, offset, weights, datasources)
  }else{
    #message("WARNING:'checks' is set to FALSE; variables in the model are not checked and error messages may not be intelligible!")
  }

#MOVE ITERATION COUNT BEFORE ASSIGNMENT OF beta.vect.next
#Iterations need to be counted. Start off with the count at 0
  #and increment by 1 at each new iteration
  iteration.count<-0

  # number of 'valid' studies (those that passed the checks) and vector of beta values
  numstudies <- length(datasources)


#ARBITRARY LENGTH FOR START BETAs AT THIS STAGE BUT IN LEGAL TRANSMISSION FORMAT ("0,0,0,0,0")
 beta.vect.next <- rep(0,5)
 beta.vect.temp <- paste0(as.character(beta.vect.next), collapse=",")


#IDENTIFY THE CORRECT DIMENSION FOR START BETAs VIA CALLING FIRST COMPONENT OF glmDS

   cally1 <- call('glmSLMADS1', formula, family, weights, offset, dataName)

   study.summary.0 <- DSI::datashield.aggregate(datasources, cally1)

at.least.one.study.data.error<-0
at.least.one.study.valid<-0


num.par.glm<-NULL
coef.names<-NULL

for(hh in 1:numstudies) {
	if(study.summary.0[[hh]]$errorMessage!="No errors")
	{
	at.least.one.study.data.error<-1
	}else{
	      at.least.one.study.valid<-1
		  num.par.glm<-study.summary.0[[hh]][[1]][[2]]
		  coef.names<-study.summary.0[[hh]][[2]]
		 }
}


y.invalid<-NULL
Xpar.invalid<-NULL
w.invalid<-NULL
o.invalid<-NULL
glm.saturation.invalid<-NULL
errorMessage<-NULL

	for(ss in 1:numstudies)
	{
	  y.invalid<-c(y.invalid,study.summary.0[[ss]][[3]])
	  Xpar.invalid<-rbind(Xpar.invalid,study.summary.0[[ss]][[4]])
          w.invalid<-c(w.invalid,study.summary.0[[ss]][[5]])
          o.invalid<-c(o.invalid,study.summary.0[[ss]][[6]])
          glm.saturation.invalid <-c(glm.saturation.invalid,study.summary.0[[ss]][[7]])
          errorMessage<-c(errorMessage,study.summary.0[[ss]][[8]])
	}


y.invalid<-as.matrix(y.invalid)
sum.y.invalid<-sum(y.invalid)
dimnames(y.invalid)<-list(names(datasources),"Y VECTOR")

Xpar.invalid<-as.matrix(Xpar.invalid)
sum.Xpar.invalid<-sum(Xpar.invalid)
dimnames(Xpar.invalid)<-list(names(datasources),coef.names)

w.invalid<-as.matrix(w.invalid)
sum.w.invalid<-sum(w.invalid)
dimnames(w.invalid)<-list(names(datasources),"WEIGHT VECTOR")

o.invalid<-as.matrix(o.invalid)
sum.o.invalid<-sum(o.invalid)
dimnames(o.invalid)<-list(names(datasources),"OFFSET VECTOR")

glm.saturation.invalid<-as.matrix(glm.saturation.invalid)
sum.glm.saturation.invalid<-sum(glm.saturation.invalid)
dimnames(glm.saturation.invalid)<-list(names(datasources),"MODEL OVERPARAMETERIZED")

errorMessage<-as.matrix(errorMessage)
dimnames(errorMessage)<-list(names(datasources),"ERROR MESSAGES")



output.blocked.information.1<-"EVERY STUDY HAS DATA THAT COULD BE POTENTIALLY DISCLOSIVE UNDER THE CURRENT MODEL:"
output.blocked.information.2<-"Any values of 1 in the following tables denote potential disclosure risks."
output.blocked.information.3<-"Please use the argument <datasources> to include only valid studies."
output.blocked.information.4<-"Errors by study are as follows:"


#CASE 1 - NO STUDIES VALID
if(!at.least.one.study.valid)
	{
	message("\n\nEVERY STUDY HAS DATA THAT COULD BE POTENTIALLY DISCLOSIVE UNDER THE CURRENT MODEL:\n",
	    "Any values of 1 in the following tables denote potential disclosure risks.\n",
		"Errors by study are as follows:\n")
#		print(as.matrix(y.invalid))
#		print(as.matrix(Xpar.invalid))
#		print(as.matrix(w.invalid))
#		print(as.matrix(o.invalid))
#		print(as.matrix(glm.saturation.invalid))
#		print(as.matrix(errorMessage))


    return(list(
		    output.blocked.information.1,
		    output.blocked.information.2,
		    output.blocked.information.4,
				y.vector.error=y.invalid,
                X.matrix.error=Xpar.invalid,
                weight.vector.error=w.invalid,
                offset.vector.error=o.invalid,
                glm.overparameterized=glm.saturation.invalid,
	        errorMessage=errorMessage
                ))
	}

#CASE 2 - AT LEAST ONE STUDY VALID AND AT LEAST ONE INVALID
if(at.least.one.study.data.error)
	{
	message("\n\nAT LEAST ONE STUDY HAS DATA THAT COULD BE POTENTIALLY DISCLOSIVE UNDER THE CURRENT MODEL:\n",
        "Any values of 1 in the following tables denote potential disclosure risks.\n",
		"No analytic results are returned for potentially disclosive studies and\n",
		"pooled co-estimates across studies are based only on the valid studies.\n",
		"You may also choose to exclude invalid studies from\n",
		"the whole analysis using the <datasources> argument.\n",
		"Errors by study are as follows:\n")
#		print(as.matrix(y.invalid))
#		print(as.matrix(Xpar.invalid))
#		print(as.matrix(w.invalid))
#		print(as.matrix(o.invalid))
#		print(as.matrix(glm.saturation.invalid))
#		print(as.matrix(errorMessage))
	
	}



   beta.vect.next <- rep(0,num.par.glm)
   beta.vect.temp <- paste0(as.character(beta.vect.next), collapse=",")


  #Provide arbitrary starting value for deviance to enable subsequent calculation of the
  #change in deviance between iterations
  dev.old<-9.99e+99

  #Convergence state needs to be monitored.
  converge.state<-FALSE

  #Define a convergence criterion. This value of epsilon corresponds to that used
  #by default for GLMs in R (see section S3 for details)
  epsilon<-1.0e-08

  f<-NULL

#NOW CALL SECOND COMPONENT OF glmDS TO GENERATE SCORE VECTORS AND INFORMATION MATRICES

    cally2 <- call('glmSLMADS2', formula, family, offset, weights, dataName)

    study.summary <- DSI::datashield.aggregate(datasources, cally2)


   numstudies<-length(datasources)

   study.include.in.analysis<-NULL
   study.with.errors<-NULL
   all.studies.valid<-1
   no.studies.valid<-1

for(j in 1:numstudies)
{
ss1<-unlist(study.summary[[j]][[1]])
if(is.numeric(ss1))
{
	inv.diag.se<-1/sqrt(diag(study.summary[[j]]$cov.scaled))

	cor.matrix<-t(diag(inv.diag.se))%*%study.summary[[j]]$cov.scaled%*%(diag(inv.diag.se))
	study.summary[[j]]$VarCovMatrix<-study.summary[[j]]$cov.scaled
	study.summary[[j]]$CorrMatrix<-cor.matrix
	study.include.in.analysis<-c(study.include.in.analysis,j)
	no.studies.valid<-0
}else{
	study.with.errors<-c(study.with.errors,j)
	all.studies.valid<-0
}

}


if(!all.studies.valid)
{
	for(sse in study.with.errors)
	    {
		cat("\n","Error report from second serverside function for study",sse,"\n")
		cat("############################################################","\n")
		cat(unlist(study.summary[[sse]][[1]]),"\n")
		cat(unlist(study.summary[[sse]][[2]]),"\n\n")

	num.messages<-length(study.summary[[sse]])-2
		for(m in 1:num.messages)
			{
			if(!is.null(unlist(study.summary[[sse]][[2+m]])))
				{
				cat(unlist(study.summary[[sse]][[2+m]]),"\n\n")
				}
			}
		}
}


#if(all.studies.valid)
#{
#		cat("\nAll studies passed disclosure tests\n\n\n")
#		}





#MAKE SURE THAT IF SOME STUDIES HAVE MORE PARAMETERS IN THE
#FITTED glm (eg BECAUSE OF ALIASING) THE FINAL RETURN MATRICES
#HAVE ENOUGH ROWS TO FIT THE MAXIMUM LENGTH


  numcoefficients.max<-0

    for(g in study.include.in.analysis){
		if(length(study.summary[[g]]$coefficients[,1])>numcoefficients.max){
		numcoefficients.max<-length(study.summary[[g]]$coefficients[,1])
		}
	}

  numcoefficients<-numcoefficients.max

  betamatrix<-matrix(NA,nrow<-numcoefficients,ncol=numstudies)
  sematrix<-matrix(NA,nrow<-numcoefficients,ncol=numstudies)


  for(k in study.include.in.analysis){
    betamatrix[,k]<-study.summary[[k]]$coefficients[,1]
    sematrix[,k]<-study.summary[[k]]$coefficients[,2]
    }

################################################
#ANNOTATE OUTPUT MATRICES WITH STUDY INDICATORS#
################################################

study.names.list<-NULL
betas.study.names.list<-NULL
ses.study.names.list<-NULL



for(v in 1:numstudies){

    study.names.list<-c(study.names.list,paste0("study",as.character(v)))
    betas.study.names.list<-c(betas.study.names.list,paste0("betas study ",as.character(v)))
  ses.study.names.list<-c(ses.study.names.list,paste0("ses study ",as.character(v)))
}

dimnames(betamatrix)<-list(dimnames(study.summary[[1]]$coefficients)[[1]], betas.study.names.list)
dimnames(sematrix)<-list(dimnames(study.summary[[1]]$coefficients)[[1]], ses.study.names.list)

output.summary.text<-paste0("list(")

for(u in 1:numstudies){
  output.summary.text<-paste0(output.summary.text,"study",as.character(u),"=study.summary[[",as.character(u),"]],"," ")
}

output.summary.text.save<-output.summary.text
output.summary.text<-paste0(output.summary.text,"input.beta.matrix.for.SLMA=as.matrix(betamatrix),input.se.matrix.for.SLMA=as.matrix(sematrix))")


output.summary<-eval(parse(text=output.summary.text))
#######################END OF ANNOTATION CODE

SLMA.pooled.ests.matrix<-matrix(NA,nrow<-numcoefficients,ncol=6)




if(!combine.with.metafor)
{
return(output.summary)
}

if(no.studies.valid)
{
return(output.summary)
}

#NOW ONLY WORKING WITH SITUATIONS WITH AT LEAST ONE VALID STUDY

#IF combine.with.metafor == TRUE, FIRST CHECK THAT THE MODELS IN EACH STUDY MATCH
#IF THERE ARE DIFFERENT NUMBERS OF PARAMETERS THE ANALYST WILL
#HAVE TO USE THE RETURNED MATRICES FOR betas AND ses TO DETERMINE WHETHER
#COMBINATION ACROSS STUDIES IS POSSIBLE AND IF SO, WHICH PARAMETERS GO WITH WHICH
#ALSO DETERMINE WHICH STUDIES HAVE VALID DATA

beta.matrix.for.SLMA<-as.matrix(betamatrix)
se.matrix.for.SLMA<-as.matrix(sematrix)

#SELECT VALID COLUMNS ONLY (THERE WILL ALWAYS BE AT LEAST ONE)

usecols<-NULL

for(ut in 1:(dim(beta.matrix.for.SLMA)[2]))
{
if(!is.na(beta.matrix.for.SLMA[1,ut])&&!is.null(beta.matrix.for.SLMA[1,ut]))
	{
	usecols<-c(usecols,ut)
	}
}


betamatrix.all<-beta.matrix.for.SLMA
sematrix.all<-se.matrix.for.SLMA

betamatrix.valid<-beta.matrix.for.SLMA[,usecols]
sematrix.valid<-se.matrix.for.SLMA[,usecols]

#CHECK FOR MATCHED PARAMETERS

  num.valid.studies<-as.numeric(dim(as.matrix(betamatrix.valid))[2])
  coefficient.vectors.match<-TRUE



if(num.valid.studies>1){
  for(j in 1:(num.valid.studies-1))
    {
    if(length(betamatrix.valid[,j])!=length(betamatrix.valid[,(j+1)]))coefficient.vectors.match<-FALSE
    }
}else{
  coefficient.vectors.match<-TRUE
     }




  if(!coefficient.vectors.match){
    cat("\n\nModels in different sources vary in structure\nplease match coefficients for meta-analysis individually\n")
	cat("nYou can use the DataSHIELD generated estimates and standard errors as the basis for a meta-analysis\nbut carry out the final pooling step independently of DataSHIELD using whatever meta-analysis package you wish\n\n")
return(list(output.summary=output.summary))
    }



#IF combine.with.metafor == TRUE AND MODEL STRUCTURES MATCH ACROSS ALL STUDIES
#CREATE STUDY LEVEL META-ANALYSIS (SLMA) ESTIMATES FOR ALL PARAMETERS
#USING metafor() AND THREE APPROACHES TO SLMA: SLMA UNDER MAXIMUM LIKELIHOOD (SMLA-ML)
#SLMA UNDER RESTRICTED MAXIMUM LIKELIHOOD (SMLA-REML) AND USING FIXED EFFECTS (SLMA-FE)

  dimnames(SLMA.pooled.ests.matrix)<-list(dimnames(betamatrix.valid)[[1]],
                                          c("pooled.ML","se.ML","pooled.REML","se.REML","pooled.FE","se.FE"))





  for(p in 1:numcoefficients){
    rma.ML<-metafor::rma(yi=as.matrix(betamatrix.valid)[p,], sei=as.matrix(sematrix.valid)[p,], method="ML")
    rma.REML<-metafor::rma(yi=as.matrix(betamatrix.valid)[p,], sei=as.matrix(sematrix.valid)[p,], method="REML")
    rma.FE<-metafor::rma(yi=as.matrix(betamatrix.valid)[p,], sei=as.matrix(sematrix.valid)[p,], method="FE")

    SLMA.pooled.ests.matrix[p,1]<-rma.ML$beta
    SLMA.pooled.ests.matrix[p,2]<-rma.ML$se

    SLMA.pooled.ests.matrix[p,3]<-rma.REML$beta
    SLMA.pooled.ests.matrix[p,4]<-rma.REML$se

    SLMA.pooled.ests.matrix[p,5]<-rma.FE$beta
    SLMA.pooled.ests.matrix[p,6]<-rma.FE$se

  }



return(list(output.summary=output.summary, num.valid.studies=num.valid.studies,betamatrix.all=betamatrix.all,sematrix.all=sematrix.all, betamatrix.valid=betamatrix.valid,sematrix.valid=sematrix.valid,
            SLMA.pooled.ests.matrix=SLMA.pooled.ests.matrix))

}

# ds.glmSLMA
