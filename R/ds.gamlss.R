#'
#' @title Generalized Additive Models for Location Scale and Shape
#' @description This function calls the gamlssDS that is a wrapper function from 
#' the gamlss R package. The function returns an object of class "gamlss", which 
#' is a generalized additive model for location, scale and shape (GAMLSS). The 
#' function also saves the residuals as an object on the server-side with a name 
#' specified by the newobj argument. In addition, if the argument centiles is set 
#' to TRUE, the function calls the centiles function from the gamlss package and 
#' returns the sample percentages below each centile curve.
#' @details For additional details see the help header of gamlss and centiles
#' functions in native R gamlss package.
#' @param formula a formula object, with the response on the left of an ~ operator, 
#' and the terms, separated by + operators, on the right. Nonparametric smoothing
#' terms are indicated by pb() for penalised beta splines, cs for smoothing splines, 
#' lo for loess smooth terms and random or ra for random terms, 
#' e.g. 'y~cs(x,df=5)+x1+x2*x3'. 
#' @param sigma.formula a formula object for fitting a model to the sigma parameter,
#' as in the formula above, e.g. sigma.formula='~cs(x,df=5)'.
#' @param nu.formula a formula object for fitting a model to the nu parameter, 
#' e.g. nu.formula='~x'.
#' @param tau.formula a formula object for fitting a model to the tau parameter, 
#' e.g. tau.formula='~cs(x,df=2)'.
#' @param family a gamlss.family object, which is used to define the distribution 
#' and the link functions of the various parameters. The distribution families 
#' supported by gamlss() can be found in gamlss.family. Functions such as 'BI()' 
#' (binomial) produce a family object. Also can be given without the parentheses
#' i.e. 'BI'. Family functions can take arguments, as in 'BI(mu.link=probit)'.
#' @param data a data frame containing the variables occurring in the formula. 
#' If this is missing, the variables should be on the parent environment.
#' @param method a character indicating the algorithm for GAMLSS. Can be either
#' 'RS', 'CG' or 'mixed'. If method='RS' the function will use the Rigby and 
#' Stasinopoulos algorithm, if method='CG' the function will use the Cole and 
#' Green algorithm, and if method='mixed' the function will use the RS algorithm
#' twice before switching to the Cole and Green algorithm for up to 10 extra
#' iterations.
#' @param mu.fix logical, indicate whether the mu parameter should be kept fixed
#' in the fitting processes.
#' @param sigma.fix logical, indicate whether the sigma parameter should be kept
#' fixed in the fitting processes.
#' @param nu.fix logical, indicate whether the nu parameter should be kept fixed 
#' in the fitting processes.
#' @param tau.fix logical, indicate whether the tau parameter should be kept fixed
#' in the fitting processes.
#' @param control this sets the control parameters of the outer iterations algorithm 
#' using the gamlss.control function. This is a vector of 7 numeric values: (i) c.crit 
#' (the convergence criterion for the algorithm), (ii) n.cyc (the number of cycles of 
#' the algorithm), (iii) mu.step (the step length for the parameter mu), (iv) sigma.step 
#' (the step length for the parameter sigma), (v) nu.step (the step length for the
#' parameter nu), (vi) tau.step (the step length for the parameter tau), (vii) gd.tol
#' (global deviance tolerance level). The default values for these 7 parameters are 
#' set to c(0.001, 20, 1, 1, 1, 1, Inf).
#' @param i.control this sets the control parameters of the inner iterations of the 
#' RS algorithm using the glim.control function. This is a vector of 4 numeric values: 
#' (i) cc (the convergence criterion for the algorithm), (ii) cyc (the number of 
#' cycles of the algorithm), (iii) bf.cyc (the number of cycles of the backfitting 
#' algorithm), (iv) bf.tol (the convergence criterion (tolerance level) for the 
#' backfitting algorithm). The default values for these 4 parameters are set to 
#' c(0.001, 50, 30, 0.001).
#' @param centiles logical, indicating whether the function centiles() will be used to 
#' tabulate the sample percentages below each centile curve. Default is set to FALSE.
#' @param xvar the unique explanatory variable used in the centiles() function. This 
#' variable is used only if the centiles argument is set to TRUE. A restriction in
#' the centiles function is that it applies to models with one explanatory variable
#' only.
#' @param newobj a character string that provides the name for the output object
#' that is stored on the data servers. Default \code{gamlss_res}. 
#' @param datasources  a list of \code{\link[DSI]{DSConnection-class}} 
#' objects obtained after login. If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#' @return a gamlss object with all components as in the native R gamlss function. 
#' Individual-level information like the components y (the response response) and 
#' residuals (the normalised quantile residuals of the model) are not disclosed to 
#' the client-side.
#' @author Demetris Avraam for DataSHIELD Development Team
#' @export
#'
ds.gamlss <- function(formula = NULL, sigma.formula = '~1', nu.formula = '~1',
                      tau.formula = '~1', family = 'NO()', data = NULL, method = 'RS', 
                      mu.fix = FALSE, sigma.fix = FALSE, nu.fix = FALSE, 
                      tau.fix = FALSE, control = c(0.001, 20, 1, 1, 1, 1, Inf),
                      i.control = c(0.001, 50, 30, 0.001), centiles = FALSE, 
                      xvar = NULL, newobj = NULL, datasources = NULL){
  
  # look for DS connections
  if(is.null(datasources)){
    datasources <- DSI::datashield.connections_find()
  }
  
  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }
  
  # verify that 'formula' was set
  if(is.null(formula)){
    stop(" Please provide a valid formula!", call.=FALSE)
  }
  
  formula <- gsub("(", "left_parenthesis", formula, fixed = TRUE)
  formula <- gsub(")", "right_parenthesis", formula, fixed = TRUE)
  formula <- gsub("~", "tilde_symbol", formula, fixed = TRUE)
  formula <- gsub("=", "equal_symbol", formula, fixed = TRUE)
  formula <- gsub(",", "comma_symbol", formula, fixed = TRUE)
  formula <- gsub(" ", "", formula, fixed = TRUE)
  
  sigma.formula <- gsub("(", "left_parenthesis", sigma.formula, fixed = TRUE)
  sigma.formula <- gsub(")", "right_parenthesis", sigma.formula, fixed = TRUE)
  sigma.formula <- gsub("~", "tilde_symbol", sigma.formula, fixed = TRUE)
  sigma.formula <- gsub("=", "equal_symbol", sigma.formula, fixed = TRUE)
  sigma.formula <- gsub(",", "comma_symbol", sigma.formula, fixed = TRUE)
  sigma.formula <- gsub(" ", "", sigma.formula, fixed = TRUE)
  
  nu.formula <- gsub("(", "left_parenthesis", nu.formula, fixed = TRUE)
  nu.formula <- gsub(")", "right_parenthesis", nu.formula, fixed = TRUE)
  nu.formula <- gsub("~", "tilde_symbol", nu.formula, fixed = TRUE)
  nu.formula <- gsub("=", "equal_symbol", nu.formula, fixed = TRUE)
  nu.formula <- gsub(",", "comma_symbol", nu.formula, fixed = TRUE)
  nu.formula <- gsub(" ", "", nu.formula, fixed = TRUE)
  
  tau.formula <- gsub("(", "left_parenthesis", tau.formula, fixed = TRUE)
  tau.formula <- gsub(")", "right_parenthesis", tau.formula, fixed = TRUE)
  tau.formula <- gsub("~", "tilde_symbol", tau.formula, fixed = TRUE)
  tau.formula <- gsub("=", "equal_symbol", tau.formula, fixed = TRUE)
  tau.formula <- gsub(",", "comma_symbol", tau.formula, fixed = TRUE)
  tau.formula <- gsub(" ", "", tau.formula, fixed = TRUE)
  
  family <- gsub("(", "left_parenthesis", family, fixed = TRUE)
  family <- gsub(")", "right_parenthesis", family, fixed = TRUE)
  family <- gsub("=", "equal_symbol", family, fixed = TRUE)
  family <- gsub(",", "comma_symbol", family, fixed = TRUE)
  family <- gsub(" ", "", family, fixed = TRUE)
  
  # check if method is either 'RS', 'CG' or 'mixed
  if(!(method %in% c("RS", "CG", "mixed"))){
    stop("Argument 'method' must be either 'RS', 'CG' or 'mixed'", call.=FALSE)
  }
  
  control <- paste0(as.character(control), collapse=",")

  i.control <- paste0(as.character(i.control), collapse=",")

  # # check if weights are defined, if yes check if are defined as a vector on the
  # # clientside or as a vector exists on the serverside
  # if(!is.null(weights)){
  #   if(source.weights == 'clientside'){
  #     if(class(weights) != 'numeric'){
  #       stop("weights should be a numeric vector", call.=FALSE)
  #     }
  #     weights <- paste0(as.character(weights), collapse=",")
  #   }else{
  #     if(source.weights == 'serverside'){
  #       isDefined(datasources, weights)
  #     }else{
  #       stop("weights should be either NULL (default), or a numeric vector specified
  #            on the clientside, or the name of a numeric vector exists on the
  #            serverside", call.=FALSE)
  #     }
  #   }
  # }else{
  #    weights <- NULL
  #    source.weights <- NULL
  # }
  
  if(centiles==TRUE & is.null(xvar)){
    stop("Provide the name of the explanatory variable in 'xvar' argument", call.=FALSE)
  }
  
  if(is.null(newobj)){
    newobj <- 'gamlss_res'
  }
  
  calltext <- call('gamlssDS', formula=formula, sigma.formula=sigma.formula, 
                   nu.formula=nu.formula, tau.formula=tau.formula,
                   family=family, data=data, method=method, mu.fix=mu.fix,
                   sigma.fix=sigma.fix, nu.fix=nu.fix, tau.fix=tau.fix,
                   control=control, i.control=i.control, centiles=centiles,
                   xvar=xvar, newobj=newobj)
  study.summary <- DSI::datashield.aggregate(datasources, calltext)
  
  return(study.summary)
  
}
