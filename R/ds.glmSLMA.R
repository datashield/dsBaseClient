#' @title ds.glmSLMA calling glmSLMADS1, glmSLMADS2
#' @description Fits a generalized linear model (glm) on data from a single or multiple sources
#' with pooled co-analysis across studies being based on study level meta-analysis
#' @details ds.glmSLMA specifies the structure of a generalized linear model (glm)
#' to be fitted separately on each study/data source. The model is first constructed
#' and disclosure checked by glmSLMADS1. This aggregate function then returns its 
#' output to ds.glmSLMA which processes the information and uses it in a call to
#' the second aggregate function glmSLMADS2. This call specifies and fits
#' the required glm in each data source.
#' Unlike glmDS2 (called by the more commonly used generalized linear modelling
#' client-side function ds.glm) the requested model is then fitted to completion
#' on the data in each study rather than iteration by iteration on all studies
#' combined. At the end of this SLMA fitting process
#' glmSLMADS2 returns study-specific parameter estimates
#' and standard errors to the client. These can then be pooled using random
#' effects (or fixed effects) meta-analysis - eg using the metafor package.
#' This mode of model fitting may
#' reasonably be called study level meta-analysis (SLMA) although the analysis
#' is based on estimates and standard errors derived from direct analysis of
#' the individual level data in each study rather than from published study
#' summaries (as is often the case with SLMA of clinical trials etc).
#' Furthermore, unlike common approaches to study-level meta-analysis
#' adopted by large multi-study research consortia (eg in the combined analysis
#' of identical genomic markers across multiple studies), the parallel
#' analyses (in every study) under ds.glmSLMA are
#' controlled entirely from one client. This avoids the time-consuming
#' need to ask each study to run its own analyses and the consequent
#' necessity to request additional work from individual studies if
#' the modelling is to be extended to include analyses not subsumed
#' in the original analytic plan. Additional analyses of this nature
#' may, for example, include analyses based on interactions between covariates
#' identified as having significant main effects in the original analysis. 
#' From a mathematical perspective, the SLMA approach (using ds.glmSLMA)
#' differs fundamentally from the usual approach using ds.glm
#' in that the latter is mathematically equivalent
#' to placing all individual-level data from all sources in
#' one central warehouse and analysing those data as one combined dataset using the
#' conventional glm() function in R. However, although this
#' may sound to be preferable under all circumstances, the SLMA approach
#' actually offers key inferential advantages when there is marked heterogeneity
#' between sources that cannot simply be corrected with fixed effects each reflecting a study
#' or centre-effect. In particular, fixed effects cannot simply be used in this way when there
#' there is heterogeneity in the effect that is of scientific interest. 
#' @param formula Denotes an object of class formula which is a character string describing
#' the model to be fitted. Most shortcut notation for formulas allowed under R's standard glm()
#' function is also allowed by ds.glmSLMA. Many glms can
#' be fitted very simply using a formula such as:
#' "y~a+b+c+d" which simply means fit a glm with y as the outcome variable with a, b, c and d as
#' covariates. By default all such models also include an intercept (regression constant) term.
#' If all you need to fit are straightforward models such as these, you do not need to read the
#' remainder of this information about "formula". But if you need to fit a more complex
#' model in a customised way, the following text gives a few additional pointers:
#' As an example, the formula: "EVENT~1+TID+SEXF*AGE.60" denotes fit a glm with the
#' variable "EVENT" as its
#' outcome with covariates TID (in this case a 6 level factor [categorical] variable denoting
#' "time period" with values between 1 and 6), SEXF (also a factor variable denoting sex
#' and AGE.60 (a quantitative variable representing age-60 in years). The term "1" forces
#' the model to include an intercept term which it would also have done by default (see above)
#' but using "1" may usefully be contrasted with using "0" (as explained below), which removes
#' the intercept term. The "*" between SEXF and AGE.60
#' means fit all possible main effects and interactions for and between those two covariates.
#' As SEXF is a factor this is equivalent to writing SEXF+AGE.60+SEXF1:AGE.60 (the
#' last element being the simple interaction term representing the product
#' of SEXF level 1 [in this case female] and AGE.60). This takes the value 0 in all males
#' (0 * AGE.60), and the same value as AGE.60 (1 * AGE.60) in females.
#' If the formula had instead been written as:
#' "EVENT~0+TID+SEXF*AGE.60" the 0 would mean do NOT fit
#' an intercept term and, because TID happens to be a six level factor this would mean
#' that the first six model parameters which were originally intercept+TID2+TID3+TID4+TID5+TID6
#' using the first formula will now become TID1+TID2+TID3+TID4+TID5+TID6.
#' This is mathematically the same model, but conveniently, it means
#' that the effect of each
#' time period may now be estimated directly. For example, the effect of time
#' period 3 is now obtained directly as the coefficient for TID3
#' rather than the sum of the coefficients for the intercept and TID3
#' which was the case using the original formula. 
#' @param family This argument identifies the error distribution function to use in
#' the model. At present
#' ds.glm has been written to fit family="gaussian" (i.e. a
#' conventional linear model with normally distributed errors), family="binomial"
#' (i.e. a conventional
#' unconditional logistic regression model), and family = "poisson" (i.e. a
#' Poisson regression model - of which perhaps the most commonly used application
#' is for survival analysis
#' using Piecewise Exponential Regression (PER) which
#' typically closely approximates Cox regression in its
#' main estimates and standard
#' errors. At present the gaussian family is automatically coupled with
#' an 'identity' link function, the binomial family with a
#' 'logistic' link function and the poisson family with a 'log' link function. For the majority of
#' applications typically
#' encountered in epidemiology and medical statistics, one  these three classes of
#' models will typically be what you need. However, if a particular user
#' wishes us to implement an alternative family
#' (e.g. 'gamma') or an alternative family/link combination (e.g. binomial with
#' probit) we can discuss how best to meet that request: it will almost certainly be possible,
#' but we may seek a small amount of funding or practical in-kind support from
#' the user in order to ensure that it can be carried outin a timely manner 
#' @param offset  A character string specifying the name of a variable to be used as
#' an offset. An offset is a component of a glm which may be viewed as a covariate
#' with a known coefficient of 1.00 and so the coefficient does not need to be 
#' estimated by the model. As an example, an offset is needed to fit a piecewise
#' exponential regression model. Unlike the standard glm() function in
#' native R, ds.glmSLMA() only allows an offset
#' to be set using the <offset> argument, it CANNOT be included directly in the
#' formula via notation
#' such as  "y~a+b+c+d+offset(offset.vector.name)". So in ds.glmSLMA this model
#' must be specified as: formula="y~a+b+c+d", ..., offset="offset.vector.name"
#' and ds.glmSLMA then incorporates
#' it appropriately into the formula itself.
#' @param weights A character string specifying the name of a variable containing
#' prior regression
#' weights for the fitting process. Like offset, ds.glmSLMA does not allow a weights vector to be
#' written directly into the glm formula. 
#' @param dataName A character string specifying the name of an (optional) dataframe that contains
#' all of the variables in the glm formula. This avoids you having to specify the name of the
#' dataframe in front of each covariate in the formula e.g. if the dataframe is
#' called "DataFrame" you 
#' avoid having to write: "DataFrame$y~DataFrame$a+DataFrame$b+DataFrame$c+DataFrame$d"
#' Processing stops if a non existing data frame is indicated. 
#' @param checks This argument is a boolean. If TRUE ds.glmSLMA then undertakes a series
#' of checks of
#' the structural integrity of the model that can take several minutes. Specifically
#' it verifies that the 
#' variables in the model are all defined (exist) on the server site at every study
#' and that they have the correct characteristics required to fit a GLM. The default
#' value is FALSE
#  because the checks markedly increase fitting time
#' and so it is suggested that the argument <checks> is only made TRUE
#' if an unexplained problem in the model fit is encountered.
#' @param maxit A numeric scalar denoting the maximum number of iterations that
#' are permitted
#' before ds.glm declares that the model has failed to converge. Logistic regression
#' and Poisson regression
#' models can require many iterations, particularly if the starting value of the
#' regression constant is
#' far away from its actual value that the glm is trying to estimate. In consequence
#' we choose to set
#' maxit=30 - but depending on the nature of the models you wish to fit, you may wish
#' to be alerted
#' more quickly than this if there is a delay in convergence, or you may wish to
#' allow MORE iterations.
#' @param combine.with.metafor This argument is Boolean. If TRUE (the default) the
#' estimates and standard errors for each regression coefficient are pooled across
#' studies using random effects meta-analysis under maximum likelihood (ML),
#' restricted maximum likelihood (REML), or fixed effects meta-analysis (FE).
#' @param datasources specifies the particular opal object(s) to use, if it is not specified
#' the default set of opals will be used. The default opals are always called default.opals.
#' This parameter is set without inverted commas: e.g. datasources=opals.em or
#' datasources=default.opals
#' If you wish to specify the second opal server in a set of three, the parameter is specified:
#' e.g. datasources=opals.em[2]. If you wish to specify the first and third opal
#' servers in a set specify:
#' e.g. datasources=opals.em[c(2,3)]
#' @return Many of the elements of the output list returned by ds.glmSLMA from
#' each study separately are precisely
#' equivalent to those returned by the glm() function in native R. However,
#' potentially disclosive elements
#' such as individual-level residuals and linear predictor values are blocked. 
#' The return results from each separate study appear first in the return list with
#' the full set of results from each study presented in a block and the
#' blocks listed in the order in which the studies appear in <datasources>.
#' As regards the elements within each study the most important
#' elements are included last in the return list because they then appear at the
#' bottom of a simple print out of the return object. In reverse order, these
#' key elements are listed below. In addition to the elements reflecting the
#' primary results of the analysis, ds.glmSLMA also returns a range of error
#' messages if the model fails indicating why failure may have occurred and
#' in particular detailing any disclosure traps that may have been 
#' @return coefficients:- a matrix in which the first column contains the names of
#' all of the regression parameters (coefficients) in the model, the second column
#' contains the estimated values of the coefficients (called estimates),
#' the third the corresponding standard errors,
#' the fourth the ratio corresponding to the value of each estimate divided by its
#' standard error and the fifth the p-value
#' treating that ratio as a standardised normal deviate (a simple Wald test).
#' @return family:- indicates the error distribution and link function used
#' in the glm
#' @return formula:- see description of formula as an input parameter (above)
#' @return df.resid:- the residual degrees of freedom around the model
#' @return deviance.resid:- the residual deviance around the model
#' @return df.null:- the degrees of freedom around the null model (with just an intercept)
#' @return dev.null:- the deviance around the null model (with just an intercept)
#' @return CorrMatrix:- the correlation matrix of parameter estimates
#' @return VarCovMatrix:- the variance covariance matrix of parameter estimates
#' @return weights:- the vector (if any) holding regression weights
#' @return offset:- the vector (if any) holding an offset (enters glm with a
#' coefficient of 1.00)
#' @return cov.scaled:- equivalent to VarCovMatrix
#' @return cov.unscaled:- equivalent to VarCovMatrix but assuming dispersion (scale)
#' parameter is 1
#' @return Nmissing:- the number of missing observations in the given study
#' @return Nvalid:- the number of valid (non-missing) observations in the given study
#' @return Ntotal:- the total number of observations in the given study (Nvalid+Nmissing)
#' @return data:- - equivalent to input parameter dataName (above)
#' @return dispersion:- - the estimated dispersion parameter: deviance.resid/df.resid for
#' a gaussian family multiple regression model, 1.00 for logistic and poisson regression
#' @return call:- - summary of key elements of the call to fit the model
#' @return na.action:- - chosen method of dealing with NAs. Usually, na.action=na.omit
#' indicating any individual (or more strictly any "observational unit")
#' that has any data missing that are needed for the model is
#' exluded from the fit, even if all the rest of the required data are present.
#' These required data include: the outcome variable, covariates,
#' or any values in a regression weight vector or offset vector. As a
#' side effect of this, when you include additional covariates in model
#' you may exclude extra individuals from the analysis
#' and this can seriously distort inferential tests based on assuming models are
#' nested (eg likelihood ratio tests).
#' @return iter:- the number of iterations required to achieve convergence
#' file for the glm() function in native R.
#' @return input.beta.matrix.for.SLMA:- a matrix containing the vector of coefficient
#' estimates from each study. In combination with the corresponding standard errors
#' (see input.se.matrix.for.SLMA) these can be imported directly into a study level
#' meta-analysis (SLMA) package such as metafor to generate estimates pooled via SLMA
#' @return input.se.matrix.for.SLMA:- a matrix containing the vector of standard error
#' estimates for coefficients from each study. In combination with the coefficients
#' (see input.beta.matrix.for.SLMA) these can be imported directly into a study level
#' meta-analysis (SLMA) package such as metafor to generate estimates pooled via SLMA
#' @return SLMA.pooled.estimates:- if the argument <combine.with.metafor> = TRUE,
#' ds.glmSLMA also returns a matrix containing pooled estimates for each
#' regression coefficient across all studies with pooling under SLMA via 
#' random effects meta-analysis under maximum likelihood (ML), restricted maximum
#' likelihood (REML) or via fixed effects meta-analysis (FE)
#' @return there are a small number of more esoteric items of information returned
#' by ds.glmSLMA. Additional information about these can be found in the help
#' @author Paul Burton for DataSHIELD Development Team
#' @export
ds.glmSLMA<-function(formula=NULL, family=NULL, offset=NULL, weights=NULL, combine.with.metafor=TRUE,dataName=NULL,
checks=FALSE, maxit=30, datasources=NULL) {

# details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
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
   
   study.summary.0 <- opal::datashield.aggregate(datasources, cally1)

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
		print(as.matrix(y.invalid))
		print(as.matrix(Xpar.invalid))
		print(as.matrix(w.invalid))
		print(as.matrix(o.invalid))
		print(as.matrix(glm.saturation.invalid))
		print(as.matrix(errorMessage))

		
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
		print(as.matrix(y.invalid))
		print(as.matrix(Xpar.invalid))
		print(as.matrix(w.invalid))
		print(as.matrix(o.invalid))
		print(as.matrix(glm.saturation.invalid))
		print(as.matrix(errorMessage))
	
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

    study.summary <- opal::datashield.aggregate(datasources, cally2)


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


if(all.studies.valid)
{
		cat("\nAll studies passed disclosure tests\n\n\n")
		}





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

