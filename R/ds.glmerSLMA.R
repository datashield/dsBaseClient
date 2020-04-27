#' @title Fitting generalized linear mixed effect models in the server-side 
#' @description \code{ds.glmerSLMA} fits a generalized linear mixed effects model
#' (GLME) on data from one or multiple sources with pooling via SLMA (study level meta-analysis).
#' @details \code{ds.glmerSLMA} fits a generalized linear mixed effects model (GLME) 
#' - e.g. a logistic or Poisson regression model including both fixed and random effects -
#'  on data from a single or multiple sources. 
#'  
#'  This function is similar to \code{glmer} function from \code{lme4} package  in native R.
#'  
#'  When there are multiple data sources, the GLME is fitted to convergence 
#'  in each data source independently. The estimates and standard errors returned
#'  to the client-side which enable cross-study pooling  using study level meta-analysis (SLMA).
#'  The SLMA used by default \code{metafor} package  
#'  but as the SLMA occurs on the client-side (a standard R environment), the user can choose 
#'  any approach to meta-analysis. Additional information about fitting GLMES 
#'  using \code{glmer} function can be obtained using R help for \code{glmer} and the \code{lme4} package. 
#' 
#' Server function called: \code{glmerSLMADS2} 
#' 
#' @param formula Denotes an R object of class formula which is a character string which describes
#' the model to be fitted. Most shortcut notation allowed by lme4's glmer() function is
#' also allowed by ds.glmerSLMA. Many glmes can be fitted very simply using a formula like:
#' "y~a+b+(1|c)" which simply means fit a glme with y as the outcome variable (e.g. 
#' a binary case-control using a logistic regression model or a count or a survival
#' time using a Poisson regression model) with a and b
#' as fixed effects, and c as a random effect or grouping factor. This allows for a random
#' intercept between groups which allows, for example, the analysis of, or correction for,
#' correlated outcomes between observational units in a group. It is also possible to fit models
#' with random slopes by specifying a model such as "y~a+b+(1+b|c)" where the effect of
#' b can vary randomly between groups defined by c.
#' Implicit nesting can be specified with formulae such as "y~a+b+(1|c/d)"
#' or "y~a+b+(1|c)+(1|c:d)". See the following for more details:
#' https://cran.r-project.org/web/packages/lme4/vignettes/lmer.pdf
#' @param offset  A character string specifying the name of a variable to be used as
#' an offset (effectively a component of the linear predictor of the glme which has a known
#' coefficient a-priori and so does not need to be estimated by the model). When using a
#' Poisson  model to fit outcome data consisting of a survival time and a censoring
#' variable (1=died, 0=alive) - e.g. when using what is called
#' 'piecewise exponential regression' - the natural log of the survival time is
#' often used as an offset. 
#' @param weights A character string specifying the name of a variable containing
#' prior regression weights for the fitting process.
#' @param combine.with.metafor This argument is Boolean. If TRUE (the default) the
#' estimates and standard errors for each regression coefficient are pooled across
#' studies using random effects meta-analysis under maximum likelihood (ML),
#' restricted maximum likelihood (REML), or fixed effects meta-analysis (FE).
#' @param dataName A character string specifying the name of an (optional) dataframe
#' that contains all of the variables in the lme formula. This avoids you having
#' to specify the name of the dataframe in front of each covariate in the formula
#' e.g. if the dataframe is called 'DataFrame' you avoid having to write: 
#' "DataFrame$y~DataFrame$a+DataFrame$b+(1|DataFrame$c)" where
#' processing stops if a non existing data frame is indicated (e.g. if you misspell the name).
#' If no dataName is specified
#' or it is specified as NULL, each variable in the formula must either sit in the top
#' level folder in the search path (ie where new objects are being placed by default)
#' or else you must explicitly specify the dataframe in which other variables sit.
#' e.g.  formula = "DataFrame1$y~DataFrame2$a+b+(1|DataFrame2$c)" if variable y is
#' in DataFrame1, variable a and c are in DataFrame2 and b is in the top level folder.
#' @param checks This argument is Boolean. If TRUE ds.glmerSLMA undertakes a series
#' of preliminary checks of structural integrity of the model. Specifically
#' it verifies that the variables in the model are all defined (exist) on the server site
#' at every study and that they have the correct characteristics (e.g. class) required to fit
#' the model. The argument defaults to FALSE because the checks markedly increase
#' the time taken to apply the function and so it is suggested that it is only made TRUE
#' if an unexplained problem in the model fit is encountered.
#' @param datasources specifies the particular opal object(s) to use. If the <datasources>
#' argument is not specified the default set of opals will be used. The default opals
#' are called default.opals and the default can be set using the function
#' {ds.setDefaultOpals.o}. If the <datasources> is to be specified, it should be set without
#' inverted commas: e.g. datasources=opals.em or datasources=default.opals. If you wish to
#' apply the function solely to e.g. the second opal server in a set of three,
#' the argument can be specified as: e.g. datasources=opals.em[2].
#' If you wish to specify the first and third opal servers in a set you specify:
#' e.g. datasources=opals.em[c(1,3)].
#' @param family This is a character string specifying the distribution of the observed
#' value of the outcome variable around the predictions generated by the linear predictor
#' as applied via the default or specified link function. Typical examples are
#' family = "binomial" for logistic regression models and family="poisson" for poisson
#' regression models. Note if you are fitting a gaussian model (a standard linear mixed
#' model you should use ds.lmerSLMA not ds.glmerSLMA. For further details about the
#' specification of family and link
#' functions in generalized linear models or generalized linear mixed models please
#' see R help for glm and glmer
#' @param control_type is an optional character string vector specifying the nature of a parameter
#' (or parameters) to be modified in the 'convergence control options' which can be viewed or
#' modified via the lme4::glmerControl function. At present only one such parameter can be modified,
#' namely the tolerance of the convergence criterion to the gradient of the log-likelihood 
#' at the maximum likelihood achieved. We have enabled this because our practical experience
#' suggests that in situations where the model looks to have converged with sensible parameter
#' values but formal convergence is not being declared, if we allow the model to be more
#' tolerant to a non-zero gradient the same parameter values are obtained but formal
#' convergence is declared. The default value for the check.conv.grad is 0.001 (note this
#' differs from the default of 0.002 for ds.lmerSLMA) but by specifying
#' control_type = "check.conv.grad" and specifying a specific value for the control_value
#' argument, you can change check.conv.grad to, for example, 0.01. If control_type is specified
#' as any character string other than "check.conv.grad" processing will stop and an error message
#' will be returned. If control_type is not specified or is declared as NULL, all control
#' parameters will take their default values (e.g. check.conv.grad will be set to 0.001).
#' If users need additional control parameters to be modified, the development team can enable
#' additional valid character strings to be specified for control_type. We have deliberately not
#' made it possible to modify any or all control parameters even though that would be relatively
#' easy. This is because it would require evaluating and then activating a potentially
#' complex text string on the serverside, which would potentially create a
#' hacking-in/disclosure risk.
#' @param control_value A numeric representing the new value which you want to allocate the
#' control parameter corresponding to the 'control-type'. At present (see control_type)
#' the only parameter this can be is the convergence tolerance "check.conv.grad". In
#' general, models will be identified as having converged more readily if the value set
#' for "check.conv.grad" is increased from its default (0.001) to,say, 0.01. Please note
#' that the risk of doing this is that the model is also more likely to be declared
#' as having converged at a local maximum that is not the global maximum likelihood.
#' This will not generally be a problem if the likelihood surface is well behaved but if
#' you have a problem with convergence you might usefully compare all the parameter
#' estimates and standard errors obtained using the default tolerance (0.001) even though
#' that has not formally converged with those obtained after convergence using the higher
#' tolerance. In our experience when the problem has simply been that a well behaved
#' model is simply failing to be declared as converged the two sets of parameters are
#' almost the same. If they are quite different you may have a badly behaved likelihood
#' surface and rather than simply allowing the model to converge by changing "check.conv.grad"
#' you probably ought to explore the likelihood surface in more detail - possibly by exploring
#' the impact of changing a range of the different control parameters via glmerControl (see
#' native R help for glmerControl). In the first instance you can potentially study this by
#' fitting the model on the data from a single study using lmer itself in native R.
#' If users need additional control parameters to be modified, the development team can enable
#' additional valid character strings to be specified for control_type. We have deliberately not
#' made it possible to modify any or all control parameters even though that would be relatively
#' easy. This is because it would require evaluating and then activating a potentially
#' complex text string on the serverside, which would potential create a
#' hacking-in/disclosure risk.
#' @param nAGQ integer scalar, defaulting to 1L. nAGQ refers to he number of points per
#' axis for evaluating the adaptive Gauss-Hermite approximation to the log-likelihood.
#' The default value of 1L, corresponds to the Laplace approximation. Values greater than
#' 1 can produce greater accuracy in the evaluation of the log-likelihood at the expense of speed
#' see also help for glmer() function in native R. A value of zero uses a faster but less exact
#' form of parameter estimation for GLMMs by optimizing the random effects and the
#' fixed-effects coefficients in the penalized iteratively reweighted least squares step.
#' In our own practical experience, particularly with very large datasets, we
#' have sometimes found it necessary TO SET nAGQ TO 0L when the model
#' appears to converge perfectly well (e.g. verbose=2 demonstrates good initial
#' convergence of both the log-likelihood and regression coefficients), but formal convergence does not get declared
#' and so no output is produced, despite running the model for many iterations and relaxing control tolerances.
#' @param verbose integer scalar. If > 0 verbose output is generated during the optimization of
#' the parameter estimates. If > 1 verbose output is generated during the individual penalized 
#' iteratively reweighted least squares (PIRLS) steps. The output is contained in each studies'
#' summary in the "iterations" slot. Default value = 0, implying no additional output.
#' @param start_theta numeric vector of length equal to number of random effects. Specify to retain
#' more control over the optimisation. See glmer() for more details.
#' @param start_fixef numeric vector of length equal to number of fixed effects (NB including intercept). 
#' Specify to retain more control over the optimisation. See glmer() for more details.
#' @param notify.of.progress specifies if console output should be produce to indicate
#' progress. The default value for notify.of.progress is FALSE.
#' @return \code{ds.glmerSLMA} returns a list of  elements mentioned 
#' below separaletly foreach study separately. 
#' @return \code{coefficients}: a matrix with 5 columns:
#'    \itemize{
#'    \item{First}{: the names of all of the regression parameters (coefficients) in the model} 
#'    \item{second}{: the estimated values} 
#'    \item{third}{: corresponding standard errors of the stimated values} 
#'    \item{fourth}{: the ratio of estimate/standard error} 
#'    \item{fifth}{: the p-value treating that as a standardised normal deviate} 
#' }
#' @return \code{CorrMatrix}: the correlation matrix of parameter estimates.
#' @return \code{VarCovMatrix}: the variance covariance matrix of parameter estimates. 
#' @return \code{weights}: the vector (if any) holding regression weights
#' @return \code{offset}: the vector (if any) holding an offset (enters glm with a coefficient of 1.0)
#' @return \code{cov.scaled}: equivalent to VarCovMatrix
#' @return \code{Nmissing}: the number of missing observations in the given study
#' @return \code{Nvalid}: the number of valid (non-missing) observations in the given study
#' @return \code{Ntotal}: the total number of observations in the given study (Nvalid+Nmissing)
#' @return \code{data}: equivalent to input parameter dataName (above)
#' @return code{call}: summary of key elements of the call to fit the model
#' @return Once the study-specific output has been returned, the function returns a
#' number of elements relating to the pooling of estimates across studies via
#' study level meta-analysis. These are as follows:
#' @return \code{input.beta.matrix.for.SLMA}: a matrix containing the vector of coefficient
#' estimates from each study. In combination with the corresponding standard errors
#' (see input.se.matrix.for.SLMA) these can be imported directly into a study level
#' meta-analysis (SLMA) package such as metafor to generate estimates pooled via SLMA
#' @return \code{input.se.matrix.for.SLMA}: a matrix containing the vector of standard error
#' estimates for coefficients from each study. In combination with the coefficients
#' (see input.beta.matrix.for.SLMA) these can be imported directly into a study level
#' meta-analysis (SLMA) package such as metafor to generate estimates pooled via SLMA
#' @return \code{SLMA.pooled.estimates}: a matrix containing pooled estimates for each
#' regression coefficient across all studies with pooling under SLMA via
#' random effects meta-analysis under maximum likelihood (ML), restricted maximum
#' likelihood (REML) or via fixed effects meta-analysis (FE)
#' @return \code{convergence.error.message}:  reports for each study whether the model converged.
#' If it did not some information about the reason for this is reported.
#' @author DataSHIELD Development Team
#' @export
ds.glmerSLMA<-function(formula=NULL, offset=NULL, weights=NULL, combine.with.metafor=TRUE,dataName=NULL,
                       checks=FALSE, datasources=NULL, family=NULL, 
					   control_type = NULL, control_value = NULL, nAGQ = 1L, verbose = 0,
					   start_theta = NULL, start_fixef = NULL, notify.of.progress=FALSE) {
  
  
 #UNDER DSi
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
  
  #formula as text, then split at pipes to avoid triggering parser
  formula <- Reduce(paste, deparse(formula))
  formula <- gsub("|", "xxx", formula, fixed = TRUE)
  formula <- gsub("(", "yyy", formula, fixed = TRUE)
  formula <- gsub(")", "zzz", formula, fixed = TRUE)
  formula <- gsub("/", "ppp", formula, fixed = TRUE)
  formula <- gsub(":", "qqq", formula, fixed = TRUE)
  formula <- gsub(" ", "", formula, fixed = TRUE)
  formula <- stats::as.formula(formula)
  #formula <- strsplit(x = formurand()la, split="|", fixed=TRUE)[[1]]
  
  # sort out the start values into transmissable format
  
  if(!is.null(start_theta)){
    theta = paste0(as.character(start_theta), collapse=",")
  }else{
    theta = NULL
  }
  if(!is.null(start_fixef)){
    fixef = paste0(as.character(start_fixef), collapse=",")
  }else{
    fixef = NULL
  }
  

#Sort out control_type and control_value
 
 if(!is.null(control_type) && is.null(control_value))
	{
	errorMessage.cv<-"ERROR: if control_type is non-null, you must specify a valid control_value eg control_value<-1.0e-7"
#	print(errorMessage.cv)
	return(list(errorMessage=errorMessage.cv))
	}

	if(!is.null(control_value))
	{
		if(is.character(control_value))
		{
		control_value.transmit<-control_value
		}else{
		control_value.transmit<-as.character(control_value)
		}
	}else{
	control_value.transmit<-NULL
	}

#check nAGQ is not null which would block the call  
 if(is.null(nAGQ))
 {
 nAGQ<-1L
 }
  
  #NOW CALL SECOND COMPONENT OF glmDS TO GENERATE SCORE VECTORS AND INFORMATION MATRICES


  calltext <- call('glmerSLMADS2', formula, offset, weights, dataName, family, 
					control_type, control_value.transmit, nAGQ, verbose, theta, fixef)
					  
  study.summary <- datashield.aggregate(datasources, calltext)



  numstudies<-length(datasources)

  numstudies<-length(datasources)

  study.include.in.analysis<-NULL
  study.with.errors<-NULL
  all.studies.valid<-1
  no.studies.valid<-1

  for(j in 1:numstudies)
  {
    ss1<-study.summary[[j]]
    if(ss1$disclosure.risk==0)
    {
      study.include.in.analysis<-c(study.include.in.analysis,j)
      no.studies.valid<-0

    }else{
      study.with.errors<-c(study.with.errors,j)
      all.studies.valid<-0
    }

  }

  if (notify.of.progress)
  {
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
    cat("\nAll studies passed disclosure tests\n")
    cat("Please check for convergence warnings in the study summaries\n\n\n")
  }
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


  if(!combine.with.metafor){
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

test.error.message<-FALSE
full.error.message<-rep("",numstudies)


for(q in 1:numstudies)
{
if(!is.null(output.summary[[q]]$optinfo$conv$lme4$messages))
	{
	test.error.message<-TRUE
	}
}  

for(q in 1:numstudies)
{
	if(is.null(output.summary[[q]]$optinfo$conv$lme4$messages))
	{
	full.error.message[q]<-paste0("Study",q,": no convergence error reported")
	}
	
	
	if(!is.null(output.summary[[q]]$optinfo$conv$lme4$messages))
	{
		full.error.message[q]<-paste0("Study",q,": ",output.summary[[q]]$optinfo$conv$lme4$messages)
	}
		
}

if (notify.of.progress)
{
cat("Convergence information\n")
for(r in 1:numstudies)
	{
    cat(full.error.message[r],"\n")
	}
}	

   return(list(output.summary=output.summary, num.valid.studies=num.valid.studies,betamatrix.all=betamatrix.all,sematrix.all=sematrix.all, betamatrix.valid=betamatrix.valid,sematrix.valid=sematrix.valid,
              SLMA.pooled.ests.matrix=SLMA.pooled.ests.matrix,Convergence.error.message=full.error.message))

}

# ds.glmerSLMA
