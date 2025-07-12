#' @title Fits Generalized Linear Model 
#' @description Fits a Generalized Linear Model (GLM) on data from single or multiple sources
#' on the server-side. 
#' @details Fits a GLM on data from a single source or multiple sources on the server-side.
#' In the latter case, the data are co-analysed (when using \code{ds.glm}) 
#' by using an approach that is mathematically equivalent to placing all individual-level
#' data from all sources in one central warehouse and analysing those data using the conventional
#' \code{glm()} function in R. In this situation marked heterogeneity between sources should be corrected
#' (where possible) with fixed effects. For example, if each study in a (binary) logistic regression
#' analysis has an independent intercept, it is equivalent to allowing each study to have a
#' different baseline risk of disease. This may also be viewed as being an IP (individual person)
#' meta-analysis with fixed effects.
#'
#' 
#' In \code{formula} most shortcut notation for formulas allowed under R's standard \code{glm()}
#' function is also allowed by \code{ds.glm}. 
#' 
#' Many GLMs can be fitted very simply using a formula such as:
#' 
#' \eqn{y~a+b+c+d}
#' 
#' which simply means fit a GLM with \code{y} as the outcome variable and 
#' \code{a}, \code{b}, \code{c} and \code{d} as covariates. 
#' By default all such models also include an intercept (regression constant) term.
#' 
#' Instead, if you need to fit a more complex
#' model, for example:
#' 
#'  \eqn{EVENT~1+TID+SEXF*AGE.60}
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

#' 
#' In the \code{family} argument can be specified three types of models to fit:
#' 
#'  \describe{
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
#' 
#' The \code{data} argument avoids you having to specify the name of the
#' data frame in front of each covariate in the formula. 
#' For example, if the data frame is called \code{DataFrame} you
#' avoid having to write: \eqn{DataFrame\$y ~ DataFrame\$a + DataFrame\$b + DataFrame\$c + DataFrame\$d}
#' 
#' The \code{checks} argument verifies that the variables in the model are all defined (exist) 
#' on the server-side at every study
#' and that they have the correct characteristics required to fit the model. 
#' It is suggested to make \code{checks} argument TRUE if an unexplained
#'  problem in the model fit is encountered because the running process takes several minutes.
#' 
#' In \code{maxit}  Logistic regression and Poisson regression
#' models can require many iterations, particularly if the starting value of the
#' regression constant is far away from its actual value that the GLM
#' is trying to estimate. In consequence we often set \code{maxit=30}
#' but depending on the nature of the models you wish to fit, you may wish
#' to be alerted much more quickly than this if there is a delay in convergence, 
#' or you may wish to all more iterations.
#' 
#'
#' Privacy protected iterative fitting of a GLM is explained here:
#'
#' (1) Begin with a guess for the coefficient vector to start iteration 1 (let's call it
#' \code{beta.vector[1]}). Using \code{beta.vector[1]}, run iteration 1 with each source
#' calculating the resultant score vector (and information matrix) generated
#' by its data - given \code{beta.vector[1]} -
#' as the sum of the score vector components (and the sum of the components of the
#' information matrix) derived from each individual data record in that source. NB in most models
#' the starting values in \code{beta.vector[1]} are set to be zero for all parameters.
#'
#' (2) Transmit the resultant score vector and information matrix from each source
#' back to the clientside
#' server (CS) at the analysis centre. Let's denote
#' \code{SCORE[1][j]} and \code{INFORMATION.MATRIX[1][j]} as the
#' score vector and information matrix generated by study \code{j} at the end of the 1st iteration.
#'
#' (3) CS sums the score vectors, and equivalently the information matrices, across all studies
#' (i.e. \code{j = 1:S}, where \code{S} is the number of studies). Note that,
#' given \code{beta.vector[1]}, this gives precisely the same final sums
#' for the score
#' vectors and information matrices as would have been obtained if all data had been in one
#' central warehoused database and the overall score vector and information matrix at the end of
#' the first iteration had been calculated
#' (as is standard) by simply summing across all individuals. The only difference is that
#' instead of directly adding all values across
#' all individuals, we first sum across all individuals in each data source and
#' then sum those study
#' totals across all studies - i.e. this generates the same ultimate sums
#'
#' (4) CS then calculates \code{sum(SCORES)\%*\% inverse(sum(INFORMATION.MATRICES))} -
#' heuristically this may be
#' viewed as being "the sum of the score vectors divided (NB 'matrix division') by the sum of the
#' information matrices". If one uses the conventional algorithm (IRLS)
#' to update generalized linear models from iteration to iteration this quantity happens to be
#' precisely the vector to be added to the
#' current value of beta.vector (i.e. \code{beta.vector[1]}) to obtain
#' \code{beta.vector[2]} which is the improved estimate of the beta.vector to be used in iteration 2.
#' This updating algorithm is often  called the IRLS (Iterative Reweighted Least
#' Squares) algorithm
#' - which is closely related to the Newton
#' Raphson approach but uses the expected information rather than
#' the observed information.
#'
#' (5) Repeat steps (2)-(4) until the model converges (using the standard R
#' convergence criterion).
#' NB An alternative way to coherently pool the glm across multiple sources is to fit each
#' glm to completion (i.e. multiple iterations until convergence) in each source and then return
#' the final parameter estimates and standard errors to the CS where they could be pooled using
#' study-level meta-analysis. An alternative function ds.glmSLMA allows you to do this.
#' It will fit the glms to completion
#' in each source and return the final estimates and standard errors (rather than score vectors
#' and information matrices). It will then rely on functions in the
#' R package metafor to meta-analyse the key parameters.
#' 
#' 
#' Server functions called: \code{glmDS1} and \code{glmDS2}
#'
#' @param formula an object of class formula describing
#' the model to be fitted. For more information see 
#' \strong{Details}. 
#' @param family identifies the error distribution function to use in
#' the model. 
#' This can be set as \code{"gaussian"}, \code{"binomial"} and \code{"poisson"}. 
#' For more information see \strong{Details}.
#' @param offset  a character string specifying the name of a variable to be used as
#' an offset. \code{ds.glm} does not allow an offset vector to be
#' written directly into the GLM formula. For more information see \strong{Details}.  
#' @param weights a character string specifying the name of a variable containing
#' prior regression weights for the fitting process. 
#' \code{ds.glm} does not allow a weights vector to be
#' written directly into the GLM formula.
#' @param data a character string specifying the name of an (optional) data frame that contains
#' all of the variables in the GLM formula. 
#' @param checks logical. If TRUE \code{ds.glm} checks the structural integrity 
#' of the model. Default FALSE. For more information see \strong{Details}.  
#' @param maxit a numeric scalar denoting the maximum number of iterations that are permitted
#' before \code{ds.glm} declares that the model has failed to converge.
#' @param CI a numeric value specifying the confidence interval. Default \code{0.95}. 
#' @param viewIter logical. If TRUE the results of the intermediate iterations are 
#' printed. If FALSE only final results are shown. Default FALSE.  
#' @param viewVarCov logical. If TRUE the variance-covariance matrix
#' of parameter estimates is returned. Default FALSE. 
#' @param viewCor logical. If TRUE the correlation matrix of
#' parameter estimates is returned. Default FALSE.
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} objects obtained after login. 
#' If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#' @return Many of the elements of the output list returned by \code{ds.glm} are 
#' equivalent to those returned by the \code{glm()} function in native R. However,
#' potentially disclosive elements
#' such as individual-level residuals and linear predictor values are blocked. 
#' In this case, only non-disclosive elements are returned from each study separately.
#' 
#' The list of elements returned by \code{ds.glm} is mentioned below: 
#' 
#' @return \code{Nvalid}: total number of valid observational units across all studies.
#' @return \code{Nmissing}: total number of observational units across all studies with at least
#'                             one data item missing.
#' @return \code{Ntotal}: total of observational units across all studies, the  
#'                           sum of valid and missing units.
#' @return \code{disclosure.risk}: risk of disclosure,
#'                                    the value 1 indicates that one of the disclosure traps 
#'                                    has been triggered in that study.
#' @return \code{errorMessage}: explanation for any errors or disclosure risks identified.
#' @return \code{nsubs}: total number of observational units used by \code{ds.glm} function. 
#'                        \code{nb} usually is the same as \code{nvalid}.
#' @return \code{iter}: total number of iterations before convergence achieved.
#' @return \code{family}: error family and link function.
#' @return \code{formula}: model formula, see description of formula as an input parameter (above).
#' @return \code{coefficients}: a matrix with 5 columns:
#'    \describe{
#'    \item{First}{: the names of all of the regression parameters (coefficients) in the model}
#'    \item{second}{: the estimated values}
#'    \item{third}{: corresponding standard errors of the estimated values}
#'    \item{fourth}{: the ratio of estimate/standard error}.
#'    \item{fifth}{: the p-value treating that as a standardised normal deviate}
#'     }
#' @return \code{dev}: residual deviance.
#' @return \code{df}: residual degrees of freedom. \code{nb} residual degrees of freedom + number of
#'    parameters in model = \code{nsubs}.
#' @return \code{output.information}: reminder to the user that there 
#'                                      is more information at the top of the output.
#' 
#' 
#' @return Also, the estimated coefficients and standard errors expanded with estimated confidence intervals
#'    with \% coverage specified by \code{ci} argument are returned. 
#'    For the poisson model,
#'    the output is generated on the scale of the linear predictor (log rates and log rate ratios) 
#'    and the natural scale after exponentiation (rates and rate ratios). 
#'
#' @author DataSHIELD Development Team
#' @export
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
#'   ds.glm(formula = EVENT ~ 1 + TID + female * age.60,
#'          data = "D",
#'          family = "poisson", 
#'          offset = "log.surv",
#'          weights = NULL,
#'          checks = FALSE,
#'          maxit = 20,
#'          CI = 0.95,
#'          viewIter = FALSE,
#'          viewVarCov = FALSE,
#'          viewCor = FALSE,
#'          datasources = connections)
#'          
#'   # Clear the Datashield R sessions and logout
#'   datashield.logout(connections) 
#'   
#'   # Example 2: run a logistic regression without interaction
#'   # For this example we are going to load another dataset  
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
#'   mod <- ds.glm(formula = "DIS_DIAB~GENDER+PM_BMI_CONTINUOUS+LAB_HDL",
#'                 data = "D",
#'                 family = "binomial",
#'                 datasources = connections)
#'                 
#'   mod #visualize the results of the model
#' 
#' # Example 3: fit a standard Gaussian linear model with an interaction
#' # We are using the same data as in example 2. 
#' 
#' mod <- ds.glm(formula = "PM_BMI_CONTINUOUS~DIS_DIAB*GENDER+LAB_HDL",
#'               data = "D",
#'               family = "gaussian",
#'               datasources = connections)
#' mod
#' 
#' # Clear the Datashield R sessions and logout
#' datashield.logout(connections) 
#' }
#'
ds.glm <- function(formula=NULL, data=NULL, family=NULL, offset=NULL, weights=NULL, checks=FALSE, maxit=20, CI=0.95,
                     viewIter=FALSE, viewVarCov=FALSE, viewCor=FALSE, datasources=NULL){

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
     sum(as.numeric(grepl('weights', formula, ignore.case=TRUE)))>0){
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

  # if the argument 'data' is set, check that the data frame is defined (i.e. exists) on the server site
  if(!(is.null(data))){
    defined <- isDefined(datasources, data)
  }

  # beginning of optional checks - the process stops if any of these checks fails #
  if(checks){
    message(" -- Verifying the variables in the model")
    # call the function that checks the variables in the formula are defined (exist) on the server site and are not missing at complete
    glmChecks(formula, data, offset, weights, datasources)
  }else{
    #message("WARNING:'checks' is set to FALSE; variables in the model are not checked and error messages may not be intelligible!")
  }

  # MOVE ITERATION COUNT BEFORE ASSIGNMENT OF beta.vect.next
  # Iterations need to be counted. Start off with the count at 0
  # and increment by 1 at each new iteration
  iteration.count <- 0

  # number of 'valid' studies (those that passed the checks) and vector of beta values
  numstudies <- length(datasources)

  # ARBITRARY LENGTH FOR START BETAs AT THIS STAGE BUT IN LEGAL TRANSMISSION FORMAT ("0,0,0,0,0")
  beta.vect.next <- rep(0,5)
  beta.vect.temp <- paste0(as.character(beta.vect.next), collapse=",")

  # IDENTIFY THE CORRECT DIMENSION FOR START BETAs VIA CALLING FIRST COMPONENT OF glmDS
  cally1 <- call('glmDS1', formula, family, weights, offset, data)
  study.summary.0 <- DSI::datashield.aggregate(datasources, cally1)

  at.least.one.study.data.error <- 0

  for(hh in 1:numstudies) {
  	if(study.summary.0[[hh]]$errorMessage!="No errors"){
  	  at.least.one.study.data.error <- 1
  	}
  }

  num.par.glm <- NULL
  coef.names <- NULL

  if(at.least.one.study.data.error==0){
    num.par.glm <- study.summary.0[[1]][[1]][[2]]
    coef.names <- study.summary.0[[1]][[2]]
  }

  y.invalid <- NULL
  Xpar.invalid <- NULL
  w.invalid <- NULL
  o.invalid <- NULL
  glm.saturation.invalid <- NULL
  errorMessage <- NULL

	for(ss in 1:numstudies){
    y.invalid<-c(y.invalid,study.summary.0[[ss]][[3]])
    Xpar.invalid<-rbind(Xpar.invalid,study.summary.0[[ss]][[4]])
    w.invalid<-c(w.invalid,study.summary.0[[ss]][[5]])
    o.invalid<-c(o.invalid,study.summary.0[[ss]][[6]])
    glm.saturation.invalid <-c(glm.saturation.invalid,study.summary.0[[ss]][[7]])
    errorMessage<-c(errorMessage,study.summary.0[[ss]][[8]])
	}

  y.invalid <- as.matrix(y.invalid)
  sum.y.invalid <- sum(y.invalid)
  dimnames(y.invalid) <- list(names(datasources), "Y VECTOR")

  Xpar.invalid <- as.matrix(Xpar.invalid)
  sum.Xpar.invalid <- sum(Xpar.invalid)
  dimnames(Xpar.invalid) <- list(names(datasources), coef.names)

  w.invalid <- as.matrix(w.invalid)
  sum.w.invalid <- sum(w.invalid)
  dimnames(w.invalid) <- list(names(datasources), "WEIGHT VECTOR")
  
  o.invalid <- as.matrix(o.invalid)
  sum.o.invalid <- sum(o.invalid)
  dimnames(o.invalid) <- list(names(datasources),"OFFSET VECTOR")
  
  glm.saturation.invalid <- as.matrix(glm.saturation.invalid)
  sum.glm.saturation.invalid <- sum(glm.saturation.invalid)
  dimnames(glm.saturation.invalid) <- list(names(datasources), "MODEL OVERPARAMETERIZED")
  
  errorMessage <- as.matrix(errorMessage)
  dimnames(errorMessage) <- list(names(datasources), "ERROR MESSAGES")
  
  output.blocked.information.1 <- "MODEL FITTING TERMINATED AT FIRST ITERATION:"
  output.blocked.information.2 <- "Any values of 1 in the following tables denote potential disclosure risks"
  output.blocked.information.3 <- "please use the argument <datasources> to include only valid studies."
  output.blocked.information.4 <- "Errors by study are as follows:"

  if(sum.y.invalid>0||sum.Xpar.invalid>0||sum.w.invalid>0||sum.o.invalid>0||sum.glm.saturation.invalid>0||at.least.one.study.data.error==1){
	  message("\n\nMODEL FITTING TERMINATED AT FIRST ITERATION:\n",
            "Any values of 1 in the following tables denote potential disclosure risks\n",
		        "please use the argument <datasources> to include only valid studies.\n",
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
      		    output.blocked.information.3,
      		    output.blocked.information.4,
      				y.vector.error=y.invalid,
              X.matrix.error=Xpar.invalid,
              weight.vector.error=w.invalid,
              offset.vector.error=o.invalid,
              glm.overparameterized=glm.saturation.invalid,
              errorMessage=errorMessage
          ))
		stop("DATA ERROR")
  }

   beta.vect.next <- rep(0, num.par.glm)
   beta.vect.temp <- paste0(as.character(beta.vect.next), collapse=",")

  # Provide arbitrary starting value for deviance to enable subsequent calculation of the
  # change in deviance between iterations
  dev.old <- 9.99e+99

  # Convergence state needs to be monitored.
  converge.state <- FALSE

  # Define a convergence criterion. This value of epsilon corresponds to that used
  # by default for GLMs in R (see section S3 for details)
  epsilon <- 1.0e-08

  f <- NULL

  while(!converge.state && iteration.count < maxit) {

    iteration.count<-iteration.count+1

    message("Iteration ", iteration.count, "...")

  # NOW CALL SECOND COMPONENT OF glmDS TO GENERATE SCORE VECTORS AND INFORMATION MATRICES
  cally2 <- call('glmDS2', formula, family, beta.vect.temp, offset, weights, data)
  study.summary <- DSI::datashield.aggregate(datasources, cally2)

  # INTEGRATE RETURNED OUTPUT
  .select <- function(l, field){
    lapply(l, function(obj) {obj[[field]]})
  }

  disclosure.risk.total <- Reduce(f="+", .select(study.summary, 'disclosure.risk'))

	disclosure.risk <- NULL
  errorMessage2 <- NULL

	for(ss2 in 1:numstudies){
    disclosure.risk <- c(disclosure.risk,study.summary[[ss]][[9]])
    errorMessage2 <- c(errorMessage2,study.summary[[ss]][[10]])
	}

  disclosure.risk <- as.matrix(disclosure.risk)
  dimnames(disclosure.risk) <- list(names(datasources), "RISK OF DISCLOSURE")

  errorMessage2 <- as.matrix(errorMessage2)
  dimnames(errorMessage2) <- list(names(datasources), "ERROR MESSAGES")

  if(disclosure.risk.total>0){
	message("Potential disclosure risk in y.vect, X.mat, w.vect or offset \n",
	        "or model overparameterized in at least one study.\n",
			"In addition clientside function appears to have been modified\n",
			"to avoid traps in first serverside function.\n",
			"Score vectors and information matrices therefore destroyed in all invalid studies\n",
			"and model fitting terminated. This error is recorded in the log file but\n",
			"please report it to the DataSHIELD team as we need to understand how\n",
			"the controlled shutdown traps in glmDS1 have been circumvented\n\n")

	output.blocked.information.1<-"Potential disclosure risk in y.vect, X.mat, w.vect or offset"
	output.blocked.information.2<-"or model overparameterized in at least one study."
	output.blocked.information.3<-"In addition clientside function appears to have been modified"
	output.blocked.information.4<-"to avoid disclosure traps in first serverside function."
	output.blocked.information.5<-"Score vectors and information matrices therefore destroyed in all invalid studies"
	output.blocked.information.6<-"and model fitting terminated. This error is recorded in the log file but"
	output.blocked.information.7<-"please also report it to the DataSHIELD team as we need to understand how"
	output.blocked.information.8<-"the controlled shutdown traps in glmDS1 have been circumvented."


        return(list(output.blocked.information.1,
			output.blocked.information.2,
			output.blocked.information.3,
			output.blocked.information.4,
			output.blocked.information.5,
			output.blocked.information.6,
			output.blocked.information.7,
			output.blocked.information.8
	            ))
	}


    info.matrix.total<-Reduce(f="+", .select(study.summary, 'info.matrix'))
    score.vect.total<-Reduce(f="+", .select(study.summary, 'score.vect'))
    dev.total<-Reduce(f="+", .select(study.summary, 'dev'))
    Nvalid.total<-Reduce(f="+", .select(study.summary, 'Nvalid'))
    Nmissing.total<-Reduce(f="+", .select(study.summary, 'Nmissing'))
    Ntotal.total<-Reduce(f="+", .select(study.summary, 'Ntotal'))

    message("CURRENT DEVIANCE:      ", dev.total)

    if(iteration.count==1) {
      # Sum participants only during first iteration.
      nsubs.total<-Reduce(f="+", .select(study.summary, 'numsubs'))
      # Save family
      f <- study.summary[[1]]$family
    }

    #Create variance covariance matrix as inverse of information matrix
    variance.covariance.matrix.total<-solve(info.matrix.total)

    # Create beta vector update terms
    beta.update.vect<-variance.covariance.matrix.total %*% score.vect.total

    #Add update terms to current beta vector to obtain new beta vector for next iteration
	if(iteration.count==1)
	{
	 beta.vect.next<-rep(0,length(beta.update.vect))
	}

	beta.vect.next<-beta.vect.next+beta.update.vect

        beta.vect.temp <- paste0(as.character(beta.vect.next), collapse=",")

    #Create a vector with the square roots of diagonal elements of variance covariance matrix
    sqrt.diagonal <- sqrt(1/diag(variance.covariance.matrix.total))

	#Calculate the correlation matrix using the variance covariance matrix
	correlation <- rep(sqrt.diagonal, dim(variance.covariance.matrix.total)[1]) * variance.covariance.matrix.total * rep(sqrt.diagonal, each = dim(variance.covariance.matrix.total)[1])

    #Calculate value of convergence statistic and test whether meets convergence criterion
    converge.value<-abs(dev.total-dev.old)/(abs(dev.total)+0.1)
    if(converge.value<=epsilon)converge.state<-TRUE
    if(converge.value>epsilon)dev.old<-dev.total

    if(viewIter){
      #For ALL iterations summarise model state after current iteration
      message("SUMMARY OF MODEL STATE after iteration ", iteration.count)
      message("Current deviance ", dev.total," on ",(nsubs.total-length(beta.vect.next)), " degrees of freedom")
      message("Convergence criterion ",converge.state," (", converge.value,")")

      message("\nbeta: ", paste(as.vector(beta.vect.next), collapse=" "))

      message("\nInformation matrix overall:")
      message(paste(utils::capture.output(info.matrix.total), collapse="\n"))

      message("\nScore vector overall:")
      message(paste(utils::capture.output(score.vect.total), collapse="\n"))

      message("\nCurrent deviance: ", dev.total, "\n")
    }
  }
  if(!viewIter){
    #For ALL iterations summarise model state after current iteration
    message("SUMMARY OF MODEL STATE after iteration ", iteration.count)
    message("Current deviance ", dev.total," on ",(nsubs.total-length(beta.vect.next)), " degrees of freedom")
    message("Convergence criterion ",converge.state," (", converge.value,")")

    message("\nbeta: ", paste(as.vector(beta.vect.next), collapse=" "))

    message("\nInformation matrix overall:")
    message(paste(utils::capture.output(info.matrix.total), collapse="\n"))

    message("\nScore vector overall:")
    message(paste(utils::capture.output(score.vect.total), collapse="\n"))

    message("\nCurrent deviance: ", dev.total, "\n")
  }

  #If convergence has been obtained, declare final (maximum likelihood) beta vector,
  #and calculate the corresponding standard errors, z scores and p values
  #(the latter two to be consistent with the output of a standard GLM analysis)
  #Then print out final model summary
  if(converge.state)
  {
    family.identified<-0

    beta.vect.final<-beta.vect.next

    scale.par <- 1
    if(f$family== 'gaussian') {
      scale.par <- dev.total / (nsubs.total-length(beta.vect.next))
    }

    family.identified<-1
    se.vect.final <- sqrt(diag(variance.covariance.matrix.total)) * sqrt(scale.par)
    z.vect.final<-beta.vect.final/se.vect.final
    pval.vect.final<-2*stats::pnorm(-abs(z.vect.final))
    parameter.names<-names(score.vect.total[,1])
    model.parameters<-cbind(beta.vect.final,se.vect.final,z.vect.final,pval.vect.final)
    dimnames(model.parameters)<-list(parameter.names,c("Estimate","Std. Error","z-value","p-value"))

    if(CI > 0)
    {
      ci.mult <- stats::qnorm(1-(1-CI)/2)
      low.ci.lp <- model.parameters[,1]-ci.mult*model.parameters[,2]
      hi.ci.lp <- model.parameters[,1]+ci.mult*model.parameters[,2]
      estimate.lp <- model.parameters[,1]



      if(family=="gaussian"){
        estimate.natural <- estimate.lp
        low.ci.natural <- low.ci.lp
        hi.ci.natural <- hi.ci.lp
        name1 <- paste0("low",CI,"CI")
        name2 <- paste0("high",CI,"CI")
        ci.mat <- cbind(low.ci.lp,hi.ci.lp)
        dimnames(ci.mat) <- list(NULL,c(name1,name2))
      }

      if(family=="binomial"){
        family.identified  <-  1
        num.parms <- length(low.ci.lp)
          name1 <- paste0("low",CI,"CI.LP")
          name2 <- paste0("high",CI,"CI.LP")
          name3 <- paste0("P_OR")
          name4 <- paste0("low",CI,"CI.P_OR")
          name5 <- paste0("high",CI,"CI.P_OR")
         estimate.natural <- exp(estimate.lp)/(1+exp(estimate.lp))
        low.ci.natural <- exp(low.ci.lp)/(1+exp(low.ci.lp))
        hi.ci.natural <- exp(hi.ci.lp)/(1+exp(hi.ci.lp))
        if(num.parms > 1){
          estimate.natural[2:num.parms] <- exp(estimate.lp[2:num.parms])
          low.ci.natural[2:num.parms] <- exp(low.ci.lp[2:num.parms])
          hi.ci.natural[2:num.parms] <- exp(hi.ci.lp[2:num.parms])
       }
        ci.mat <- cbind(low.ci.lp,hi.ci.lp,estimate.natural,low.ci.natural,hi.ci.natural)
        dimnames(ci.mat) <- list(NULL,c(name1,name2,name3,name4,name5))

      }

      if(family=="poisson"){
        family.identified <- 1
        num.parms <- length(low.ci.lp)
        estimate.natural <- exp(estimate.lp)
        low.ci.natural <- exp(low.ci.lp)
        hi.ci.natural <- exp(hi.ci.lp)
        name1 <- paste0("low",CI,"CI.LP")
        name2 <- paste0("high",CI,"CI.LP")
        name3 <- paste0("EXPONENTIATED RR")
        name4 <- paste0("low",CI,"CI.EXP")
        name5 <- paste0("high",CI,"CI.EXP")
        ci.mat <- cbind(low.ci.lp,hi.ci.lp,estimate.natural,low.ci.natural,hi.ci.natural)
        dimnames(ci.mat) <- list(NULL,c(name1,name2,name3,name4,name5))
      }

      if(family.identified==0)
      {
        estimate.natural <- estimate.lp
        low.ci.natural <- low.ci.lp
        hi.ci.natural <- hi.ci.lp
        name1 <- paste0("low",CI,"CI")
        name2 <- paste0("high",CI,"CI")
        ci.mat <- cbind(low.ci.lp,hi.ci.lp)
        dimnames(ci.mat) <- list(NULL,c(name1,name2))
      }

    }

    model.parameters<-cbind(model.parameters,ci.mat)

    if(!is.null(offset)&&!is.null(weights)){
       formulatext <- paste0(Reduce(paste, deparse(formula)), paste0(" + offset(", offset, ")"), paste0(" + weights(", weights, ")"))
       }
    if(!is.null(offset)&&is.null(weights)){
       formulatext <- paste0(Reduce(paste, deparse(formula)), paste0(" + offset(", offset, ")"))
       }
    if(is.null(offset)&&!is.null(weights)){
       formulatext <- paste0(Reduce(paste, deparse(formula)), paste0(" + weights(", weights, ")"))
       }

    if(is.null(offset)&&is.null(weights)){
       formulatext <- Reduce(paste, deparse(formula))
       }

    if(!viewVarCov & !viewCor){
      glmds <- list(
			Nvalid=Nvalid.total,
			Nmissing=Nmissing.total,
			Ntotal=Ntotal.total,
			disclosure.risk=disclosure.risk,
            errorMessage=errorMessage2,
            nsubs=nsubs.total,
            iter=iteration.count,
            family=f,
            formula=formulatext,
            coefficients=model.parameters,
            dev=dev.total,
            df=(nsubs.total-length(beta.vect.next)),
            output.information="SEE TOP OF OUTPUT FOR INFORMATION ON MISSING DATA AND ERROR MESSAGES"
        )

#   class(glmds) <- 'glmds'

        return(glmds)
	}

    if(viewVarCov & viewCor){
      glmds <- list(
			Nvalid=Nvalid.total,
			Nmissing=Nmissing.total,
			Ntotal=Ntotal.total,
			disclosure.risk=disclosure.risk,
                        errorMessage=errorMessage2,
	                VarCovMatrix=variance.covariance.matrix.total,
			CorrMatrix=correlation,
			nsubs=nsubs.total,
                        iter=iteration.count,
                        family=f,
                        formula=formulatext,
                        coefficients=model.parameters,
                        dev=dev.total,
                        df=(nsubs.total-length(beta.vect.next)),
			output.information="SEE TOP OF OUTPUT FOR INFORMATION ON MISSING DATA AND ERROR MESSAGES"
                 )
#   class(glmds) <- 'glmds'
			return(glmds)
    }
	if(!viewVarCov & viewCor){
	      glmds <- list(
			Nvalid=Nvalid.total,
			Nmissing=Nmissing.total,
			Ntotal=Ntotal.total,
			disclosure.risk=disclosure.risk,
                        errorMessage=errorMessage2,
			CorrMatrix=correlation,
			nsubs=nsubs.total,
                        iter=iteration.count,
	                family=f,
                        formula=formulatext,
                        coefficients=model.parameters,
                        dev=dev.total,
                        df=(nsubs.total-length(beta.vect.next)),
		        output.information="SEE TOP OF OUTPUT FOR INFORMATION ON MISSING DATA AND ERROR MESSAGES"
                )
        return(glmds)
    }


    if(viewVarCov & !viewCor){
	      glmds <- list(
			Nvalid=Nvalid.total,
			Nmissing=Nmissing.total,
			Ntotal=Ntotal.total,
			disclosure.risk=disclosure.risk,
                        errorMessage=errorMessage2,
	                VarCovMatrix=variance.covariance.matrix.total,
			nsubs=nsubs.total,
                        iter=iteration.count,
	                family=f,
                        formula=formulatext,
                        coefficients=model.parameters,
                        dev=dev.total,
                        df=(nsubs.total-length(beta.vect.next)),
			output.information="SEE TOP OF OUTPUT FOR INFORMATION ON MISSING DATA AND ERROR MESSAGES"
                )
        return(glmds)
    }

  } else {
    warning(paste("Did not converge after", maxit, "iterations. Increase maxit parameter as necessary."))
    return(NULL)
  }

}
#ds.glm
