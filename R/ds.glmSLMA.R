#' @title Fit a Generalized Linear Model (GLM) with pooling via Study Level Meta-Analysis (SLMA)
#' @description Fits a generalized linear model (GLM) on data from single or multiple sources
#' with pooled co-analysis across studies being based on SLMA (Study Level Meta Analysis). 
#' @details \code{ds.glmSLMA} specifies the structure of a Generalized Linear Model 
#' to be fitted separately on each study or data source. Calls serverside functions
#' glmSLMADS1 (aggregate),glmSLMADS2 (aggregate) and glmSLMADS.assign (assign). 
#' From a mathematical perspective, the SLMA approach (using \code{ds.glmSLMA})
#' differs fundamentally from the alternative approach using \code{ds.glm}.
#' ds.glm fits the model iteratively across all studies together. At each
#' iteration the model in every data source has precisely the same coefficients
#' so when the model converges one essentially identifies the model that
#' best fits all studies simultaneously. This mathematically equivalent
#' to placing all individual-level data from all sources in
#' one central warehouse and analysing those data as one combined dataset using the
#' conventional \code{glm()} function in native R. In contrast ds.glmSLMA sends
#' a command to every data source to fit the model required but each separate source
#' simply fits that model to completion (ie undertakes all iterations until
#' the model converges) and the estimates (regression coefficients) and their standard
#' errors from each source are sent back to the client and are then pooled using SLMA
#' via any approach the user wishes to implement. The ds.glmSLMA functions includes
#' an argument <combine.with.metafor> which if TRUE (the default) pools the models
#' across studies using the metafor function (from the metafor package) using three
#' optimisation methods: random effects under maximum likelihood (ML); random effects
#' under restricted maximum likelihood (REML); or fixed effects (FE). But once
#' the estimates and standard errors are on the clientside, the user
#' can alternatively choose to use the metafor package in any way he/she wishes,
#' to pool the coefficients across studies or, indeed, to use another
#' meta-analysis package, or their own code.
#' 
#' Although the ds.glm approach might at first sight appear to be preferable
#' under all circumstances, this is not always the case.
#' First, the results from both approaches are generally very
#' similar. Secondly, the SLMA approach can offer key inferential advantages
#' when there is marked heterogeneity between sources that cannot simply be corrected
#' by including fixed-effects in one's ds.glm model that each reflect
#' a study- or centre-specific effect. In particular, such fixed effects cannot
#' be guaranteed to generate formal inferences that
#' are unbiased when there is heterogeneity
#' in the effect that is actually of scientific interest. It might be argued that
#' one should not try to pool the inferences anyway if there is marked heterogeneity,
#' but you can use the joint analysis to formally check for such heterogeneity and then
#' choose to report the pooled result or separate results from each study individually.
#' Crucially, unless the heterogeneity is substantial, pooling can be quite reasonable.
#' Furthermore, if you just fit a ds.glm model without centre-effects you will
#' in effect be pooling across all studies without checking for heterogeneity and
#' if heterogeneity exists and if it is strong you can get theoretically results
#' that are badly confounded by study. Before we introduced ds.glmSLMA we
#' encountered a real world example of a ds.glm (without centre effects)
#' which generated combined inferences over all studies which were more extreme than
#' the results from any of the individual studies: the lower 95% confidence limit
#' of the combined estimate was higher than the upper 95% confidence limits in
#' ALL of the individual studies. This was clearly incorrect and provided a
#' salutary lesson on the potential impact of confounding by study if a ds.glm
#' model does not include appropriate centre-effects. Even if you are going
#' to undertake a ds.glm analysis (which is slightly more powerful when there
#' is no heterogeneity) it may still be useful to also carry out a ds.glmSLMA
#' analysis as this provides a very easy way to examine the extent of heterogeneity.
#'
#' In \code{formula} Most shortcut notation for formulas allowed under R's standard \code{glm()}
#' function is also allowed by \code{ds.glmSLMA}. 
#' 
#' Many glms can be fitted very simply using a formula such as:
#' 
#' \eqn{y~a+b+c+d}
#' 
#' which simply means fit a glm with \code{y} as the outcome variable and 
#' \code{a}, \code{b}, \code{c} and \code{d} as covariates. 
#' By default all such models also include an intercept (regression constant) term.
#' 
#' Instead, if you need to fit a more complex
#' model, for example:
#' 
#' \eqn{EVENT~1+TID+SEXF*AGE.60}
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
#' In the \code{family} argument a range of model types can be fitted. This range has recently
#' been extended to include a number of model types that are non-standard but are used
#' relatively widely.
#' 
#' The standard models include:
#'  \describe{
#'    \item{\code{"gaussian"}}{: conventional linear model with normally distributed errors} 
#'    \item{\code{"binomial"}}{: conventional unconditional logistic regression model}
#'    \item{\code{"poisson"}}{: Poisson regression model which is often used in epidemiological
#'     analysis of counts and rates and is also used in survival analysis. The
#'     Piecewise Exponential Regression (PER) model typically provides a close approximation
#'     to the Cox regression model in its main estimates and standard errors.}
#'    \item{\code{"gamma"}}{: a family of models for outcomes characterised by a constant
#'     coefficient of variation, i.e. the variance increases with the square of the expected mean}

#'
#'	The extended range includes:
#'    \item{\code{"quasipoisson"}}{: a model with a Poisson variance function - variance
#'     equals expected mean - but the residual variance which is fixed to be 1.00 in
#'     a standard Poisson model can then take any value. This is achieved by a dispersion parameter
#'     which is estimated during the model fit and if it takes the value K it means
#'	   that the expected variance is K x the expected mean, which implies that all
#'     standard errors will be sqrt(K) times larger than in a standard Poisson model
#'     fitted to the same data. This allows for the extra uncertainty which is associated 
#'     with 'overdispersion' that occurs very commonly with Poisson distributed data,
#'     and typically arises when the count/rate data being modelled occur in blocks
#'	   which exhibit heterogeneity of underlying risk which is not being fully
#'     modelled, either by including the blocks themselves as a factor or by including
#'     covariates for all the determinants that are relevant to that underlying risk. If
#'     there is no overdispersion (K=1) the estimates and standard errors from the
#'     quasipoisson model will be almost identical to those from a standard poisson model.}
#' 
#'    \item{\code{"quasibinomial"}}{: a model with a binomial variance function - if P
#'     is the expected proportion of successes, and N is the number of "trials" (always
#'     1 if analysing binary data which are formally described as having a Bernoulli
#'     distribution (binomial distribution with N=1) the variance function is \code{N*(P)*(1-P)}.
#'     But the residual variance which is fixed to be 1.00 in
#'     a binomial model can take any value. This is achieved by a dispersion parameter
#'     which is estimated during the model fit (see quasipoisson information above).}}

#' Each class of models has a "canonical link" which represents the link function that
#' maximises the information extraction by the model. The gaussian family uses the
#' \code{identity} link, the poisson family the \code{log} link, the binomial/Bernoulli family the
#' \code{logit} link and the the gamma family the \code{reciprocal} link.
#' 
#' The \code{dataName} argument avoids you having to specify the name of the
#' data frame in front of each covariate in the formula. 
#' For example, if the data frame is called \code{DataFrame} you
#' avoid having to write: \eqn{DataFrame\$y ~ DataFrame\$a + DataFrame\$b + DataFrame\$c + DataFrame\$d}
#' 
#' The \code{checks} argument verifies that the variables in the model are all defined (exist) 
#' on the server-site at every study
#' and that they have the correct characteristics required to fit the model. 
#' It is suggested to make \code{checks} argument TRUE only if an unexplained
#'  problem in the model fit is encountered because the running process takes several minutes.
#'  
#' In \code{maxit} Logistic regression and Poisson regression
#' models can require many iterations, particularly if the starting value of the
#' regression constant is far away from its actual value that the GLM
#' is trying to estimate. In consequence we often set \code{maxit=30}
#' but depending on the nature of the models you wish to fit, you may wish
#' to be alerted much more quickly than this if there is a delay in convergence, 
#' or you may wish to allow more iterations.
#' 
#' 
#' Server functions called: \code{glmSLMADS1}, \code{glmSLMADS2}, \code{glmSLMADS.assign}
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
#' @param combine.with.metafor logical. If TRUE the
#' estimates and standard errors for each regression coefficient are pooled across
#' studies using random-effects meta-analysis under maximum likelihood (ML),
#' restricted maximum likelihood (REML) or fixed-effects meta-analysis (FE). Default TRUE. 
#' @param newobj a character string specifying the name of the object to which the glm object
#' representing the model fit on the serverside in each study is to be written.
#' If no <newobj> argument is specified, the output
#' object defaults to "new.glm.obj". 
#' @param dataName a character string specifying the name of an (optional) data frame
#' that contains all of the variables in the GLM formula. 
#' @param checks logical. If TRUE \code{ds.glmSLMA} checks the structural integrity 
#' of the model. Default FALSE. For more information see \strong{Details}.
#' @param maxit a numeric scalar denoting the maximum number of iterations that
#' are permitted before \code{ds.glmSLMA} declares that the model has failed to converge. 
#' For more information see \strong{Details}.
#' @param notify.of.progress specifies if console output should be produced to indicate
#' progress. Default FALSE.
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} objects obtained after login. 
#' If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#' @return The serverside aggregate functions \code{glmSLMADS1} and \code{glmSLMADS2} return
#' output to the clientside, while the assign function \code{glmSLMADS.assign} simply writes
#' the glm object to the serverside
#' created by the model fit on a given server as a permanent object on that same server.
#' This is precisely
#' the same as the glm object that is usually created by a call to glm() in native R and it
#' contains all the same elements (see help for glm in native R). Because it is a serverside
#' object, no disclosure blocks apply. However, such disclosure blocks do apply to the information
#' passed to the clientside. In consequence, rather than containing all the components of a
#' standard glm object in native R, the components of the glm object that are returned by
#' \code{ds.glmSLMA} include: a mixture of non-disclosive elements of the glm object
#' reported separately by study included in a list object called \code{output.summary}; and
#' a series of other list objects that represent inferences aggregated across studies.
#' @return the study specific items include: 
#' @return \code{coefficients}: a matrix with 5 columns:
#'    \describe{
#'    \item{First}{: the names of all of the regression parameters (coefficients) in the model} 
#'    \item{second}{: the estimated values} 
#'    \item{third}{: corresponding standard errors of the estimated values} 
#'    \item{fourth}{: the ratio of estimate/standard error} 
#'    \item{fifth}{: the p-value treating that as a standardised normal deviate} 
#' }
#' @return \code{family}: indicates the error distribution and link function used
#' in the GLM.
#' @return \code{formula}: model formula, see description of formula as an input parameter (above).
#' @return \code{df.resid}: the residual degrees of freedom around the model.
#' @return \code{deviance.resid}: the residual deviance around the model.
#' @return \code{df.null}: the degrees of freedom around the null model (with just an intercept).
#' @return \code{dev.null}: the deviance around the null model (with just an intercept).
#' @return \code{CorrMatrix}: the correlation matrix of parameter estimates.
#' @return \code{VarCovMatrix}: the variance-covariance matrix of parameter estimates.
#' @return \code{weights}: the name of the vector (if any) holding regression weights.
#' @return \code{offset}: the name of the vector (if any) holding an offset (enters glm with a
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
#' @return \code{na.action}:  chosen method of dealing with missing values. This is 
#' usually, \code{na.action = na.omit} - see help in native R.
#' @return \code{iter}: the number of iterations required to achieve convergence
#' of the glm model in each separate study.
#' @return Once the study-specific output has been returned, \code{ds.glmSLMA}
#' returns a series of lists relating to the aggregated inferences across studies.
#' These include the following:
#' @return \code{num.valid.studies}: the number of studies with valid output
#' included in the combined analysis
#' @return \code{betamatrix.all}: matrix with a row for each regression coefficient
#' and a column for each study reporting the estimated regression coefficients
#' by study. 
#' @return \code{sematrix.all}: matrix with a row for each regression coefficient
#' and a column for each study reporting the standard errors of the estimated
#' regression coefficients by study. 
#' @return \code{betamatrix.valid}: matrix with a row for each regression coefficient
#' and a column for each study reporting the estimated regression coefficients
#' by study but only for studies with valid output (eg not violating disclosure traps) 
#' @return \code{sematrix.all}: matrix with a row for each regression coefficient
#' and a column for each study reporting the standard errors of the estimated
#' regression coefficients by study but only for studies with valid output
#' (eg not violating disclosure traps)
#' @return \code{SLMA.pooled.estimates.matrix}: a matrix with a row for each
#' regression coefficient and six columns. The first two columns contain the
#' pooled estimate of each regression coefficients and its standard error with
#' pooling via random effect meta-analysis under maximum likelihood (ML). Columns
#' 3 and 4 contain the estimates and standard errors from random effect meta-analysis
#' under REML and columns 5 and 6 the estimates and standard errors under fixed
#' effect meta-analysis. This matrix is only returned if the
#' argument combine.with.metafor is set to TRUE. Otherwise, users can take
#' the \code{betamatrix.valid} and \code{sematrix.valid} matrices and enter
#' them into their meta-analysis package of choice.
#' @return \code{is.object.created} and \code{validity.check} are standard
#' items returned by an assign function when the designated newobj appears to have
#' been successfully created on the serverside at each study. This output is
#' produced specifically by the assign function \code{glmSLMADS.assign} that writes
#' out the glm object on the serverside 
#' @author Paul Burton, for DataSHIELD Development Team 07/07/20
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
#'          dataName = "D",
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
#'                 dataName = "D",
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
#'               dataName = "D",
#'               family = "gaussian",
#'               datasources = connections)
#' mod
#' 
#' # Clear the Datashield R sessions and logout
#' datashield.logout(connections) 
#' }
#'
#' @export
ds.glmSLMA<-function(formula=NULL, family=NULL, offset=NULL, weights=NULL, combine.with.metafor=TRUE,
	newobj=NULL,dataName=NULL,checks=FALSE, maxit=30, notify.of.progress=FALSE, datasources=NULL) {

  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
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
       message("\n\n WARNING: you may have specified an offset or regression weights")
       message("\n as part of the model formula. In ds.glm (unlike the usual glm in R)")
       message("\n you must specify an offset or weights separately from the formula")
       message("\n using the offset or weights argument.\n\n")
}

  formula <- stats::as.formula(formula)


  # check that 'family' was set
  if(is.null(family)){
    stop(" Please provide a valid 'family' argument!", call.=FALSE)
  }

#####################
  #if family is non-standard (i.e.family includes calls to either link or variance) convert to transmittable form:
  #quasi link=identity, variance = constant

  if(paste(strsplit(family,split=" ")[[1]],collapse="")=="quasi(link=identity,variance=constant)"||
	 paste(strsplit(family,split=" ")[[1]],collapse="")=="quasi(link=identity)"||
	 paste(strsplit(family,split=" ")[[1]],collapse="")=="quasi(variance=constant)")
	 
	 {family<-"quasi"}

#  quasipoisson
  if(paste(strsplit(family,split=" ")[[1]],collapse="")=="quasipoisson(link=log)"||
	 paste(strsplit(family,split=" ")[[1]],collapse="")=="quasi(link=log,variance=mu)")	 
	 {family<-"quasipoisson"}

#  quasibinomial
  if(paste(strsplit(family,split=" ")[[1]],collapse="")=="quasibinomial(link=logit)"||
	 paste(strsplit(family,split=" ")[[1]],collapse="")=="quasi(link=logit,variance=mu(1-mu))")	 
	 {family<-"quasibinomial"}

#  poisson
     if(paste(strsplit(family,split=" ")[[1]],collapse="")=="poisson(link=log)"||
	 paste(strsplit(family,split=" ")[[1]],collapse="")=="poisson(link=log,variance=mu)")	 
	 {family<-"poisson"}
	 
#  binomial
     if(paste(strsplit(family,split=" ")[[1]],collapse="")=="binomial(link=logit)"||
	 paste(strsplit(family,split=" ")[[1]],collapse="")=="binomial(link=logit,variance=mu(1-mu))")	 
	 {family<-"binomial"}
	 
	 
#SPECIAL COMBINATIONS (NEED TO MODIFY SERVERSIDE FUNCTION TO ADD THEM)
#similar to gamma with a log link
  if(paste(strsplit(family,split=" ")[[1]],collapse="")=="quasi(link=log,variance=mu^2)")
 	 {family<-"quasigamma.link_log"}
 
  if(paste(strsplit(family,split=" ")[[1]],collapse="")=="Gamma(link=log)")
 	 {family<-"Gamma.link.log"}

  if(paste(strsplit(family,split=" ")[[1]],collapse="")=="gamma(link=log)")
 	 {family<-"Gamma.link.log"}
  
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
#####################

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

   calltext.1 <- call('glmSLMADS1', formula, family, weights, offset, dataName)
  
   study.summary.0 <- DSI::datashield.aggregate(datasources, calltext.1)

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

#NOW CALL assign COMPONENT OF glmSLMADS TO FIT AND SAVE glm MODEL
#OBJECT ON SERVERSIDE

# if no value specified for glm output object, then specify a default
		if(is.null(newobj)){
		newobj <- "new.glm.obj"
		}

   if (notify.of.progress) {
       message("\n\nSAVING SERVERSIDE glm OBJECT AS: <",newobj,">\n\n")
   }

   calltext.2 <- call('glmSLMADS.assign', formula, family, offset, weights, dataName)

   DSI::datashield.assign(datasources, newobj, calltext.2)

#NOW CALL MODIFIED AGGREGATE FUNCTION "glmSLMADS2" TO CREATE SUMMARY TO USE FOR META-ANALYSIS
#AND RETURN TO CLIENTSIDE

    calltext.3 <- call('glmSLMADS2', formula, family, offset, weights, newobj, dataName)

    study.summary <- DSI::datashield.aggregate(datasources, calltext.3)

#NOW USE ALL ORIGINAL PROCESSING CODE TO GET THE SUMMARY OBJECT INTO THE CORRECT FORMAT

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
		message("\n","Error report from second serverside function for study",sse,"\n")
		message("############################################################","\n")
		message(unlist(study.summary[[sse]][[1]]),"\n")
		message(unlist(study.summary[[sse]][[2]]),"\n\n")

	num.messages<-length(study.summary[[sse]])-2
		for(m in 1:num.messages)
			{
			if(!is.null(unlist(study.summary[[sse]][[2+m]])))
				{
				message(unlist(study.summary[[sse]][[2+m]]),"\n\n")
				}
			}
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
    message("\n\nModels in different sources vary in structure\nplease match coefficients for meta-analysis individually\n")
	message("nYou can use the DataSHIELD generated estimates and standard errors as the basis for a meta-analysis\nbut carry out the final pooling step independently of DataSHIELD using whatever meta-analysis package you wish\n\n")
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


#final.outlist<-(list(output.summary=output.summary, num.valid.studies=num.valid.studies,betamatrix.all=betamatrix.all,sematrix.all=sematrix.all, betamatrix.valid=betamatrix.valid,sematrix.valid=sematrix.valid,
#            SLMA.pooled.ests.matrix=SLMA.pooled.ests.matrix))


#############################################################################################################
#DataSHIELD CLIENTSIDE MODULE: CHECK KEY DATA OBJECTS SUCCESSFULLY CREATED                                  #
																											#
#SET APPROPRIATE PARAMETERS FOR THIS PARTICULAR FUNCTION                                                 	#
test.obj.name<-newobj																					 	#
																											#
#TRACER																									 	#
#return(test.obj.name)																					 	#
#}                                                                                   					 	#
																											#
																											#							
# CALL SEVERSIDE FUNCTION                                                                                	#
calltext <- call("testObjExistsDS", test.obj.name)													 		#
																											#
object.info<-datashield.aggregate(datasources, calltext)
																											#
																											#
# CHECK IN EACH SOURCE WHETHER OBJECT NAME EXISTS														 	#
# AND WHETHER OBJECT PHYSICALLY EXISTS WITH A NON-NULL CLASS											 	#
num.datasources<-length(object.info)																	 	#
																											#
																											#
obj.name.exists.in.all.sources<-TRUE																	 	#
obj.non.null.in.all.sources<-TRUE																		 	#
																											#
for(j in 1:num.datasources){																			 	#
	if(!object.info[[j]]$test.obj.exists){																 	#
		obj.name.exists.in.all.sources<-FALSE															 	#
		}																								 	#
	if("ABSENT" %in% object.info[[j]]$test.obj.class){														#
		obj.non.null.in.all.sources<-FALSE																 	#
		}																								 	#
	}																									 	#
																											#
if(obj.name.exists.in.all.sources && obj.non.null.in.all.sources){										    #	
																											#
	return.message<-																					 	#
    paste0("A data object <", test.obj.name, "> has been created in all specified data sources")		 	#
																											#
																											#
	}else{																								 	#
																											#
    return.message.1<-																					 	#
	paste0("Error: A valid data object <", test.obj.name, "> does NOT exist in ALL specified data sources")	#
																											#
	return.message.2<-																					 	#
	paste0("It is either ABSENT and/or has no valid content/class,see return.info above")				 	#
																											#
	return.message.3<-																					 	#
	paste0("Please use ds.ls() to identify where missing")												 	#
																											#
																											#
	return.message<-list(return.message.1,return.message.2,return.message.3)							 	#
																											#
	}																										#
																											#
	calltext <- call("messageDS", test.obj.name)															#
    studyside.message<-datashield.aggregate(datasources, calltext)											#
																											#	
	no.errors<-TRUE																							#
	for(nd in 1:num.datasources){																			#
		if(studyside.message[[nd]]!="ALL OK: there are no studysideMessage(s) on this datasource"){			#
		no.errors<-FALSE																					#
		}																									#
	}																										#	
																											#
																											#
	if(no.errors){																							#
	validity.check<-paste0("<",test.obj.name, "> appears valid in all sources")							    #
	return(list(output.summary=output.summary, num.valid.studies=num.valid.studies,							#
	betamatrix.all=betamatrix.all,																			#
	sematrix.all=sematrix.all, betamatrix.valid=betamatrix.valid,sematrix.valid=sematrix.valid,				#
    SLMA.pooled.ests.matrix=SLMA.pooled.ests.matrix,														#
	is.object.created=return.message,validity.check=validity.check))						    			#
	}																										#
																											#
if(!no.errors){																								#
	validity.check<-paste0("<",test.obj.name,"> invalid in at least one source. See studyside.messages:")   #
	return(list(is.object.created=return.message,validity.check=validity.check,					    		#
	            studyside.messages=studyside.message))			                                            #
	}																										#
																											#
#END OF CHECK OBJECT CREATED CORRECTLY MODULE															 	#
#############################################################################################################

}

# ds.glmSLMA
