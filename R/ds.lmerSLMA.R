#' @title Fits Linear Mixed-Effect model via Study-Level Meta-Analysis
#' @description \code{ds.lmerSLMA} fits a Linear Mixed-Effects Model (lme) - can include both
#' fixed and random-effects - on data from one or multiple sources with pooling via SLMA
#' (Study-Level Meta-Analysis)
#' @details \code{ds.lmerSLMA} fits a Linear Mixed Effects Model (lme) - can include both fixed and random
#' effects - on data from single or multiple sources.
#' 
#' This function is similar to \code{lmer} function from \code{lme4} package in native R.
#' 
#' When there are multiple data sources,
#' the LME is fitted to convergence in each data source independently. The
#' estimates and standard errors returned to the client-side which enable cross-study pooling
#' using Study-Level Meta-Analysis (SLMA).  The SLMA used by default \code{metafor} package  
#'  but as the SLMA occurs on the client-side (a standard R environment), the user can choose 
#'  any approach to meta-analysis.  Additional information about fitting 
#' LMEs using the \code{lmer} function can be 
#' obtained using R help for \code{lmer} and the \code{lme4} package.
#' 
#' In \code{formula} most shortcut notation allowed by  \code{lmer()} function is
#' also allowed by \code{ds.lmerSLMA}. Many LMEs can be fitted very simply using a formula like:
#' \eqn{y ~ a + b + (1 | c)}
#' which simply means fit an LME with \code{y} as the outcome variable with \code{a} and \code{b}
#' as fixed effects, and \code{c} as a random effect or grouping factor.
#' 
#' It is also possible to fit models with random slopes by specifying a model such as 
#' \eqn{y ~ a + b + (1 + b | c)}
#' where the effect of \code{b} can vary randomly between groups defined by \code{c}.
#' Implicit nesting can be specified with formulae such as \eqn{y ~ a + b + (1 | c / d)}
#' or \eqn{y ~ a + b + (1 | c) + (1 | c : d)}.
#' 
#' The \code{dataName} argument avoids you having to specify the name of the
#' data frame in front of each covariate in the formula. 
#' For example, if the data frame is called \code{DataFrame} you avoid having to write: 
#' \eqn{DataFrame\$y ~ DataFrame\$a + DataFrame\$b + (1 | DataFrame\$c)}.
#' 
#' The \code{checks} argument verifies that the variables in the model are all defined (exist) 
#' on the server-site at every study
#' and that they have the correct characteristics required to fit the model. 
#' It is suggested to make \code{checks} argument TRUE if an unexplained
#' problem in the model fit is encountered because the running process takes several minutes.
#' 
#' \code{REML} can help to mitigate
#' bias associated with the fixed-effects. See help on the \code{lmer()} function for more details.
#' 
#' In \code{control_type} at present only one such parameter can be modified,
#' namely the tolerance of the convergence criterion to the gradient of the log-likelihood 
#' at the maximum likelihood achieved. We have enabled this because our practical experience
#' suggests that in situations where the model looks to have converged with sensible parameter
#' values but formal convergence is not being declared if we allow the model to be more
#' tolerant to a non-zero gradient the same parameter values are obtained but formal
#' convergence is declared. The default value for the \code{check.conv.grad} is \code{0.002}. 
#' 
#' \code{control_value} At present (see \code{control_type})
#' the only parameter this can be is the convergence tolerance \code{check.conv.grad}. In
#' general, models will be identified as having converged more readily if the value set
#' for \code{check.conv.grad} is increased from its default (\code{0.002}).  Please note
#' that the risk of doing this is that the model is also more likely to be declared
#' as having converged at a local maximum that is not the global maximum likelihood.
#' This will not generally be a problem if the likelihood surface is well behaved but if
#' you have a problem with convergence you might usefully compare all the parameter
#' estimates and standard errors obtained using the default tolerance (\code{0.002}) even though
#' that has not formally converged with those obtained after convergence using the higher
#' tolerance. 
#' 
#' The \code{optimizer} argument is built in but it won't do anything because there is only one
#' standard optimizer available for lmer - this is the \code{nloptwrap} optimizer. If users
#' wish to apply a different optimizer - potentially one they have developed themselves -
#' the development team can activate this argument so alternatives can be specified.
#' 
#' Server function called: \code{lmerSLMADS2} 
#' 
#' @param formula an object of class formula describing the model to be fitted. 
#' For more information see \strong{Details}.
#' @param offset  a character string specifying the name of a variable to be used as
#' an offset.
#' @param weights a character string specifying the name of a variable containing
#' prior regression weights for the fitting process.
#' @param combine.with.metafor logical. If TRUE the
#' estimates and standard errors for each regression coefficient are pooled across
#' studies using random-effects meta-analysis under maximum likelihood (ML),
#' restricted maximum likelihood (REML) or fixed-effects meta-analysis (FE). Default TRUE.
#' @param dataName a character string specifying the name of an (optional) data frame
#' that contains all of the variables in the LME formula. 
#' For more information see \strong{Details}.
#' @param checks logical. If TRUE \code{ds.lmerSLMA} checks the structural integrity 
#' of the model. Default FALSE. For more information see \strong{Details}.  
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} objects obtained after login. 
#' If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#' @param REML logical. If TRUE the REstricted Maximum Likelihood (REML)
#' is used for parameter optimization. 
#' If FALSE the parameters are optimized using standard ML (maximum likelihood). Default TRUE. 
#' For more information see \strong{Details}. 
#' @param control_type an optional character string vector specifying the nature of a parameter
#' (or parameters) to be modified in the \code{convergence control options} which can be viewed or
#' modified via the \code{lmerControl} function of the package \code{lme4}. 
#' For more information see \strong{Details}.  
#' @param control_value numeric representing the new value which you want to allocate the
#' control parameter corresponding to the \code{control-type}. 
#' For more information see \strong{Details}.
#' @param optimizer specifies the parameter optimizer that \code{lmer} should use.
#' For more information see \strong{Details}.
#' @param verbose an integer value. If \eqn{verbose > 0} the output is generated during the optimization of
#' the parameter estimates. If \eqn{verbose > 1} the output is generated during the individual penalized 
#' iteratively reweighted least squares (PIRLS) steps. Default \code{verbose} 
#'  value is 0 which means no additional output. 
#' @param notify.of.progress specifies if console output should be produced to indicate
#' progress. Default FALSE.
#' @param assign a logical, indicates whether the function will call a second server-side function
#' (an assign) in order to save the regression outcomes (i.e. a lmerMod object) on each server.
#' Default FALSE.
#' @param newobj a character string specifying the name of the object to which the lmerMod object
#' representing the model fit on the serverside in each study is to be written. This argument is 
#' used only when the argument \code{assign} is set to TRUE.
#' If no <newobj> argument is specified, the output object defaults to "new.lmer.obj". 
#' @return Many of the elements of the output list returned by \code{ds.lmerSLMA} are 
#' equivalent to those returned by the \code{lmer()} function in native R. However,
#' potentially disclosive elements
#' such as individual-level residuals and linear predictor values are blocked. 
#' In this case, only non-disclosive elements are returned from each study separately.
#' 
#' The list of elements returned by \code{ds.lmerSLMA} is mentioned below: 
#' 
#' @return \code{ds.lmerSLMA} returns a list of  elements mentioned 
#' below separately for each study.  
#' @return \code{coefficients}: a matrix with 5 columns:
#'    \describe{
#'    \item{First}{: the names of all of the regression parameters (coefficients) in the model} 
#'    \item{second}{: the estimated values} 
#'    \item{third}{: corresponding standard errors of the estimated values} 
#'    \item{fourth}{: the ratio of estimate/standard error} 
#'    \item{fifth}{: the p-value treating that as a standardised normal deviate} 
#' }
#' @return \code{CorrMatrix}: the correlation matrix of parameter estimates.
#' @return \code{VarCovMatrix}: the variance-covariance matrix of parameter estimates.
#' @return \code{weights}: the vector (if any) holding regression weights.
#' @return \code{offset}: the vector (if any) holding an offset.
#' @return \code{cov.scaled}: equivalent to \code{VarCovMatrix}.
#' @return \code{Nmissing}: the number of missing observations in the given study.
#' @return \code{Nvalid}: the number of valid (non-missing) observations in the given study.
#' @return \code{Ntotal}: the total number of observations 
#'                        in the given study (\code{Nvalid} + \code{Nmissing}).
#' @return \code{data}: equivalent to input parameter \code{dataName} (above).
#' @return \code{call}: summary of key elements of the call to fit the model.
#' @return There are a small number of more esoteric items of the information returned
#' by \code{ds.lmerSLMA}. Additional information about these can be found in the help
#' file for the \code{lmer()} function in the \code{lme4} package.
#' @return Once the study-specific output has been returned, the function returns
#' several elements relating to the pooling of estimates across studies via
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
#'   builder <- DSI::newDSLoginBuilder()
#'   builder$append(server = "study1", 
#'                  url = "http://192.168.56.100:8080/", 
#'                  user = "administrator", password = "datashield_test&", 
#'                  table = "CLUSTER.CLUSTER_SLO1", driver = "OpalDriver")
#'   builder$append(server = "study2", 
#'                  url = "http://192.168.56.100:8080/", 
#'                  user = "administrator", password = "datashield_test&", 
#'                  table = "CLUSTER.CLUSTER_SLO2", driver = "OpalDriver")
#'   builder$append(server = "study3",
#'                  url = "http://192.168.56.100:8080/", 
#'                  user = "administrator", password = "datashield_test&", 
#'                  table = "CLUSTER.CLUSTER_SLO3", driver = "OpalDriver")
#'   logindata <- builder$build()
#'   
#'    #Log onto the remote Opal training servers
#'    connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D")
#'                
#'   # Select all rows without missing values
#'   ds.completeCases(x1 = "D", newobj = "D.comp", datasources = connections)
#'  
#'   # Fit the lmer
#'   
#'   ds.lmerSLMA(formula = "BMI ~ incid_rate + diabetes + (1 | Male)",
#'                dataName = "D.comp",
#'                datasources = connections)
#'   
#'   # Clear the Datashield R sessions and logout
#'   datashield.logout(connections) 
#'   }
#' @export
ds.lmerSLMA <- function(formula=NULL, offset=NULL, weights=NULL, combine.with.metafor=TRUE,dataName=NULL,
                       checks=FALSE, datasources=NULL, REML=TRUE, control_type = NULL, 
                      control_value = NULL, optimizer = NULL, verbose = 0, notify.of.progress=FALSE,
                      assign=FALSE, newobj=NULL){

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
    cat("\n\n WARNING: you may have specified an offset or regression weights")
    cat("\n as part of the model formula. In ds.glm (unlike the usual glm in R)")
    cat("\n you must specify an offset or weights separately from the formula")
    cat("\n using the offset or weights argument.\n\n")
  }
  
  formula <- stats::as.formula(formula)
  
  # set family to gaussian
  family <- 'gaussian'
  
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

#Sort out control_type and control_value
 

 if(!is.null(control_type) && is.null(control_value))
	{
	errorMessage.cv<-"ERROR: if control_type is non-null, you must specify a valid control_value eg control_value<-1.0e-7"
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
 
 if(!is.null(optimizer)&&optimizer!="nloptwrap")
        {
	errorMessage.opt<-"ERROR: the only optimizer currently available for lmer is 'nloptwrap', please respecify"
	cat("\n",errorMessage.opt,"\n")
	return(list(errorMessage=errorMessage.opt))
	}

 
  #NOW CALL SECOND COMPONENT OF glmDS TO GENERATE SCORE VECTORS AND INFORMATION MATRICES

  calltext <- call('lmerSLMADS2', formula, offset, weights, dataName, REML,
                    control_type, control_value.transmit, optimizer, verbose)
  
  if(assign==TRUE){
    
    if(is.null(newobj)){
      newobj <- "new.lmer.obj"
    }
    
    if (notify.of.progress) {
        cat("\n\nSAVING SERVERSIDE lmerMod OBJECT AS: <",newobj,">\n\n")
    }
    
    calltext.2 <- call('lmerSLMADS.assign', formula, offset, weights, dataName, REML,
                       control_type, control_value.transmit, optimizer, verbose)
    
    DSI::datashield.assign(datasources, newobj, calltext.2)
    
  }
 
  study.summary <- datashield.aggregate(datasources, calltext)

  numstudies <- length(datasources)

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

# ds.lmerSLMA
