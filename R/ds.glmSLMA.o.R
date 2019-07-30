#' 
#' @title ds.glmSLMA.o calling glmDS1.o, glmSLMADS2.o
#' @description Fits a generalized linear model (glm) on data from a single or multiple sources
#' @details Fits a glm on data from a single source or from multiple sources. In the latter case 
#' (when using ds.glmSLMA.o), the glm is fitted to convergence in each data source and the
#' estimates and standard errors
#' returned from each study separately. When these are then pooled using a function such as 
#' ds.metafor, this is a form of study level meta-analysis (SLMA). This contrasts with the setting
#' when ds.glm is used to fit a glm to multiple sources. In that case the update at each
#' iteration uses data from all sources simultaneously and returns the updated parameter
#' estimates to all sources to start the next iteration. The approach (using ds.glm) is
#' mathematically equivalent to placing all individual-level data from all sources in
#' one central warehouse and analysing those data as one simultaneous block using the conventional
#' glm() function in R. The SLMA approach offers some advantages when there is marked heterogeneity
#' between sources that cannot simply be corrected with fixed effects each reflecting a study
#' or centre effect. In particular fixed effects cannot simply be used in this way when there
#' there is heterogeneity in the effect of scientific interest. 
#' @param formula Denotes an object of class formula which is a character string which describes
#' the model to be fitted. Most shortcut notation allowed by R's standard glm() function is
#' also allowed by ds.glmSLMA. Many glms can be fitted very simply using a formula like:
#' "y~a+b+c+d" which simply means fit a glm with y as the outcome variable with a, b, c and d as
#' covariates. By default all such models also include an intercept (regression constant) term.
#' If all you need to fit are straightforward models such as these, you do not need to read the
#' remainder of this information about "formula". But if you need to fit a more complex model in a
#' customised way, the next paragraph gives a few additional pointers.
#' As an example, the formula: "EVENT~1+TID+SEXF*AGE.60" denotes fit a glm with the
#' variable "EVENT" as its
#' outcome with covariates TID (in this case a 6 level factor [categorical] variable denoting
#' "time period" with values between 1 and 6), SEXF (also a factor variable denoting sex
#' and AGE.60 (a quantitative variable representing age-60 in years). The term "1" forces
#' the model to include an intercept term which it would also have done by default (see above)
#' but using "1" may usefully be contrasted with using "0" (as explained below).
#' The "*" between SEXF and AGE.60
#' means fit all possible main effects and interactions for and between those two covariates.
#' As SEXF is a factor this is equivalent to writing SEXF+AGE.60+SEXF1:AGE.60 (the
#' last element being
#' the interaction term representing the product of SEXF level 1 (in this case female)
#' and AGE.60).
#' If the formula had instead been written as :
#' "EVENT~0+TID+SEXF*AGE.60" the 0 would mean do NOT fit
#' an intercept term and, because TID happens to be a six level factor this would mean
#' that the first six model parameters which were originally intercept+TID2+TID3+TID4+TID5+TID6
#' using the first formula will now become TID1+TID2+TID3+TID4+TID5+TID6.
#' Conveniently, this means
#' that the effect of each
#' time period may now be estimated directly. For example, the effect of time
#' period 3 is now obtained
#' directly as TID3 rather than intercept+TID3 as was the case using the original formula. 
#' @param family This argument identifies the error distribution function to use in
#' the model. At present
#' ds.glm has been written to fit family="gaussian" (i.e. a
#' conventional linear model, family="binomial"
#' (i.e. a conventional
#' unconditional logistic regression model), and family = "poisson" (i.e. a
#' Poisson regression model - of which perhaps the most commonly used application
#' is for survival analysis
#' using Piecewise Exponential Regression (PER) which
#' typically closely approximates Cox regression in its
#' main estimates and standard
#' errors. More information about PER can be found in the help folder for
#' the ds.lexis function which sets up the data structure for a PER. At present the
#' gaussian family is
#' automatically coupled with an 'identity' link function, the binomial family with a
#' 'logistic' link
#' function and the poisson family with a 'log' link function. For the majority of
#' applications typically
#' encountered in epidemiology and medical statistics, one  these three classes of
#' models will
#' generally be what you need. However, if a particular user wishes us to implement
#' an alternative family
#' (e.g. 'gamma') or an alternative family/link combination (e.g. binomial with
#' probit) we can discuss
#' how best to meet that request: it will almost certainly be possible, but we may
#' seek a small amount
#' of funding or practical programming support from the user in order to ensure that
#' it can be carried out
#' in a timely manner 
#' @param offset  A character string specifying the name of a variable to be used as
#' an offset (effectively
#' a component of the glm which has a known coefficient a-priori and so does not need to be 
#' estimated by the model). As an example, an offset is needed to fit a piecewise
#' exponential regression model. Unlike the standard glm() function in R, ds.glm()
#' only allows an offset
#' to be set using the offset= argument, it CANNOT be included directly in the
#' formula via notation
#' such as  "y~a+b+c+d+offset(offset.vector.name)". In ds.glmSLMA this must be specified as:
#' formula="y~a+b+c+d", ..., offset="offset.vector.name" and ds.glmSLMA then incorporates
#' it appropriately
#' into the formula itself.
#' @param weights A character string specifying the name of a variable containing
#' prior regression
#' weights for the fitting process. Like offset, ds.glmSLMA does not allow a weights vector to be
#' written directly into the glm formula. Using weights provides an alternative way
#' to fit PER models
#' if you want to avoid using an offset, but this approach may be viewed as less 'elegant'
#' @param dataName A character string specifying the name of an (optional) dataframe that contains
#' all of the variables in the glm formula. This avoids you having to specify the name of the
#' dataframe in front of each covariate in the formula e.g. if the dataframe is
#' called 'DataFrame' you 
#' avoid having to write: "DataFrame$y~DataFrame$a+DataFrame$b+DataFrame$c+DataFrame$d"
#' Processing stops if a non existing data frame is indicated. 
#' @param checks This argument is a boolean. If TRUE ds.glm then undertakes a series
#' of checks of
#' the structural integrity of the model that can take several minutes. Specifically
#' it verifies that the 
#' variables in the model are all defined (exist) on the server site at every study
#' and that they have the correct characteristics required to fit a GLM. The default
#' value is FALSE
#  because the checks markedly increase fitting time
#' and so it is suggested that they are only made TRUE
#' if an unexplained problem in the model fit is encountered.
#' @param maxit A numeric scalar denoting the maximum number of iterations that are permitted
#' before ds.glm declares that the model has failed to converge. Logistic regression
#' and Poisson regression
#' models can require many iterations, particularly if the starting value of the
#' regression constant is
#' far away from its actual value that the glm is trying to estimate. In consequence
#' we choose to set
#' maxit=30 - but depending on the nature of the models you wish to fit, you may wish
#' to be alerted
#' much more quickly than this if there is a delay in convergence, or you may wish to
#' allow MORE iterations.
#' @param combine.with.metafor This argument is Boolean. If TRUE (the default) the
#' estimates and standard errors for each regression coefficient are pooled across
#' studies using random effects meta-analysis under maximum likelihood (ML),
#' restricted maximum likelihood (REML), or fixed effects meta-analysis (FE).
#' @param datasources specifies the particular opal object(s) to use, if it is not specified
#' the default set of opals will be used. The default opals are always called default.opals.
#' This parameter is set without inverted commas: e.g. datasources=opals.em or datasources=default.opals
#' If you wish to specify the second opal server in a set of three, the parameter is specified:
#' e.g. datasources=opals.em[2]. If you wish to specify the first and third opal servers in a set specify:
#' e.g. datasources=opals.em[2,3]
#' @return many of the elements of the output list returned by ds.glmSLMA.o from
#' each study separately are 
#' equivalent to those from glm() in native R with potentially disclosive elements
#' such as individual-level residuals and linear predictors blocked. 
#' The return results from each study appear first in the return list with one
#' block of results from each study in the order they appear in datasources.
#' As regards the elements within each study the most important
#' elements are included last in the return list because they then appear at the
#' bottom of a simple print out of the return object. In reverse order, these
#' key elements in reverse order are:
#' @return coefficients:- a matrix in which the first column contains the names of
#' all of the regression parameters (coefficients) in the model, the second column
#' contains the estimated values, the third their corresponding standard errors,
#' the fourth the ratio of estimate/standard error and the fifth the p-value
#' treating that as a standardised normal deviate
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
#' @return offset:- the vector (if any) holding an offset (enters glm with a coefficient of 1.0)
#' @return cov.scaled:- equivalent to VarCovMatrix
#' @return cov.unscaled:- equivalent to VarCovMatrix but assuming dispersion (scale) parameter is 1
#' @return Nmissing:- the number of missing observations in the given study
#' @return Nvalid:- the number of valid (non-missing) observations in the given study
#' @return Ntotal:- the total number of observations in the given study (Nvalid+Nmissing)
#' @return data:- - equivalent to input parameter dataName (above)
#' @return dispersion:- - the estimated dispersion parameter: deviance.resid/df.resid for
#' a gaussian family multiple regression model, 1.0 for logistic and poisson regression
#' @return call:- - summary of key elements of the call to fit the model
#' @return na.action:- - chosen method of dealing with NAs. Commonly na.action=nam.omit
#' indicating any individual with any data missing that are needed for the model is
#' exluded from the fit. This includes the outcome variable, covariates,
#' or any values in a regression weight vector or offset vector. By including
#' more covariates in a model you may delete extra individuals from an analysis
#' and this can severely distort inferential tests based on assuming models are
#' nested (eg likelihood ratio tests)
#' @return iter:- the number of iterations required to achieve convergence
#' @return there are a small number of more esoteric items of information returned
#' by ds.glmSLMA.o. Additional information about these can be found in the help
#' file for the glm() function in native R.
#' @return input.beta.matrix.for.SLMA:- a matrix containing the vector of coefficient
#' estimates from each study. In combination with the corresponding standard errors
#' (see input.se.matrix.for.SLMA) these can be imported directly into a study level
#' meta-analysis (SLMA) package such as metafor to generate estimates pooled via SLMA
#' @return input.se.matrix.for.SLMA:- a matrix containing the vector of standard error
#' estimates for coefficients from each study. In combination with the coefficients
#' (see input.beta.matrix.for.SLMA) these can be imported directly into a study level
#' meta-analysis (SLMA) package such as metafor to generate estimates pooled via SLMA
#' @return SLMA.pooled.estimates:- a matrix containing pooled estimates for each
#' regression coefficient across all studies with pooling under SLMA via 
#' random effects meta-analysis under maximum likelihood (ML), restricted maximum
#' likelihood (REML) or via fixed effects meta-analysis (FE)
#' @author DataSHIELD Development Team
#' @seealso \link{ds.lexis} for survival analysis using piecewise exponential regression
#' @export
ds.glmSLMA.o<-function(formula=NULL, family=NULL, offset=NULL, weights=NULL, combine.with.metafor=TRUE,dataName=NULL,
checks=FALSE, maxit=15, datasources=NULL) {

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
  
   cally1 <- call('glmDS1.o', formula, family, weights, dataName)
   
   study.summary.0 <- opal::datashield.aggregate(datasources, cally1)


at.least.one.study.data.error<-0

for(hh in 1:numstudies) {
if(study.summary.0[[hh]]$errorMessage!="No errors"){
at.least.one.study.data.error<-1
}
}


num.par.glm<-NULL
coef.names<-NULL

if(at.least.one.study.data.error==0){
num.par.glm<-study.summary.0[[1]][[1]][[2]]
coef.names<-study.summary.0[[1]][[2]]
}

y.invalid<-NULL
Xpar.invalid<-NULL
w.invalid<-NULL
glm.saturation.invalid<-NULL
errorMessage<-NULL

for(ss in 1:numstudies)
{
  y.invalid<-c(y.invalid,study.summary.0[[ss]][[3]])
  Xpar.invalid<-rbind(Xpar.invalid,study.summary.0[[ss]][[4]])
     w.invalid<-c(w.invalid,study.summary.0[[ss]][[5]])
      glm.saturation.invalid <-c(glm.saturation.invalid,study.summary.0[[ss]][[6]])
      errorMessage<-c(errorMessage,study.summary.0[[ss]][[7]])
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

glm.saturation.invalid<-as.matrix(glm.saturation.invalid)
sum.glm.saturation.invalid<-sum(glm.saturation.invalid)
dimnames(glm.saturation.invalid)<-list(names(datasources),"MODEL OVERPARAMETERIZED")

errorMessage<-as.matrix(errorMessage)
dimnames(errorMessage)<-list(names(datasources),"ERROR MESSAGES")



output.blocked.information.1<-"MODEL FITTING TERMINATED AT FIRST ITERATION:"
output.blocked.information.2<-"ANY VALUES OF 1 IN THE FOLLOWING TABLES DENOTE"
output.blocked.information.3<-"POTENTIAL DISCLOSURE RISKS. PLEASE USE THE ARGUMENT"
output.blocked.information.4<-"[datasources=] TO EXCLUDE STUDIES WITH DATA ERRORS"




if(sum.y.invalid>0||sum.Xpar.invalid>0||sum.w.invalid>0||sum.glm.saturation.invalid>0||at.least.one.study.data.error==1){
    return(list(
    output.blocked.information.1,
    output.blocked.information.2,
    output.blocked.information.3,
    output.blocked.information.4,
    y.vector.error=y.invalid,
                X.matrix.error=Xpar.invalid,
                weight.vector.error=w.invalid,
                glm.overparameterized=glm.saturation.invalid,
        errorMessage=errorMessage
                ))
stop("DATA ERROR") 
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
 
    cally2 <- call('glmSLMADS2.o', formula, family, offset, weights, dataName)

    study.summary <- opal::datashield.aggregate(datasources, cally2)

   numstudies<-length(datasources)

  for(j in 1:numstudies){
	inv.diag.se<-1/sqrt(diag(study.summary[[j]]$cov.scaled))
	
	cor.matrix<-t(diag(inv.diag.se))%*%study.summary[[j]]$cov.scaled%*%(diag(inv.diag.se))
	study.summary[[j]]$VarCovMatrix<-study.summary[[j]]$cov.scaled
	study.summary[[j]]$CorrMatrix<-cor.matrix
  }


#ARRANGE betas AND ses AS RETURN OBJECTS TO FEED EASILY
#INTO A RANDOM EFFECTS META-ANALYSIS FUNCTION SUCH AS metafor


#MAKE SURE THAT IF SOME STUDIES HAVE MORE PARAMETERS IN THE 
#FITTED glm (eg BECAUSE OF ALIASING) THE FINAL RETURN MATRICES
#HAVE ENOUGH ROWS TO FIT THE MAXIMUM LENGTH
  

  numcoefficients.max<-0
  
    for(g in 1:numstudies){		
		if(length(study.summary[[g]]$coefficients[,1])>numcoefficients.max){
		numcoefficients.max<-length(study.summary[[g]]$coefficients[,1])
		}
	}
	
  numcoefficients<-numcoefficients.max
  
  betamatrix<-matrix(NA,nrow<-numcoefficients,ncol=numstudies)
  sematrix<-matrix(NA,nrow<-numcoefficients,ncol=numstudies)
  
  
  for(k in 1:numstudies){
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
			   

if(!combine.with.metafor){
return(output.summary)
}


#IF combine.with.metafor == TRUE, FIRST CHECK THAT THE MODELS IN EACH STUDY MATCH
#IF THERE ARE DIFFERENT NUMBERS OF PARAMETERS THE ANALYST WILL
#HAVE TO USE THE RETURNED MATRICES FOR betas AND ses TO DETERMINE WHETHER
#COMBINATION ACROSS STUDIES IS POSSIBLE AND IF SO, WHICH PARAMETERS GO WITH WHICH
if(combine.with.metafor){
  numstudies<-length(datasources)
  coefficient.vectors.match<-TRUE
  for(j in 1:(numstudies-1)){
    if(dim(study.summary[[j]]$coefficients)[1]!=dim(study.summary[[(j+1)]]$coefficients)[1])coefficient.vectors.match<-FALSE
    }
  if(!coefficient.vectors.match){
    cat("\n\nModels in different sources vary in structure\nplease match coefficients for meta-analysis individually\n\n")
    return(output.summary)
    }
  
#IF combine.with.metafor == TRUE AND MODEL STRUCTURES MATCH ACROSS ALL STUDIES
#CREATE STUDY LEVEL META-ANALYSIS (SLMA) ESTIMATES FOR ALL PARAMETERS
#USING metafor() AND THREE APPROACHES TO SLMA: SLMA UNDER MAXIMUM LIKELIHOOD (SMLA-ML)
#SLMA UNDER RESTRICTED MAXIMUM LIKELIHOOD (SMLA-REML) AND USING FIXED EFFECTS (SLMA-FE)

  dimnames(SLMA.pooled.ests.matrix)<-list(dimnames(study.summary[[1]]$coefficients)[[1]],
                                          c("pooled.ML","se.ML","pooled.REML","se.REML","pooled.FE","se.FE"))
  
  for(p in 1:numcoefficients){
    rma.ML<-metafor::rma(yi=betamatrix[p,], sei=sematrix[p,], method="ML")
    rma.REML<-metafor::rma(yi=betamatrix[p,], sei=sematrix[p,], method="REML")
    rma.FE<-metafor::rma(yi=betamatrix[p,], sei=sematrix[p,], method="FE")

    SLMA.pooled.ests.matrix[p,1]<-rma.ML$beta
    SLMA.pooled.ests.matrix[p,2]<-rma.ML$se
    
    SLMA.pooled.ests.matrix[p,3]<-rma.REML$beta
    SLMA.pooled.ests.matrix[p,4]<-rma.REML$se

    SLMA.pooled.ests.matrix[p,5]<-rma.FE$beta
    SLMA.pooled.ests.matrix[p,6]<-rma.FE$se
    
  }

}

output.summary.plus.pooled.SLMA.text<-paste0(output.summary.text.save,
     "input.beta.matrix.for.SLMA=as.matrix(betamatrix),input.se.matrix.for.SLMA=as.matrix(sematrix),SLMA.pooled.estimates=SLMA.pooled.ests.matrix)")


output.summary.plus.pooled.SLMA<-eval(parse(text=output.summary.plus.pooled.SLMA.text))


  return(output.summary.plus.pooled.SLMA)
}

# ds.glmSLMA.o 
