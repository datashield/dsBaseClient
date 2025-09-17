#' @title Forestplot for SLMA models
#' @description Draws a forestplot of the coefficients for Study-Level Meta-Analysis performed with
#' DataSHIELD
#'
#' @param mod \code{list} List outputted by any of the SLMA models of DataSHIELD (\code{ds.glmerSLMA}, 
#' \code{ds.glmSLMA}, \code{ds.lmerSLMA})
#' @param variable \code{character} (default \code{NULL}) Variable to meta-analyse and visualise, by setting this 
#' argument to \code{NULL} (default) the first independent variable will be used.
#' @param method \code{character} (Default \code{"ML"}) Method to estimate the between study variance. 
#' See details from \code{?meta::metagen} for the different options.
#' @param layout \code{character} (default \code{"JAMA"}) Layout of the plot. 
#' See details from \code{?meta::metagen} for the different options.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   # Run a logistic regression
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
#'   # Plot the results of the model
#'   ds.forestplot(mod)
#' }
#' 

ds.forestplot <- function(mod, variable = NULL, method = "ML", layout = "JAMA"){
  
  # Extract meta-information from mod object
  betas <- mod$output.summary$input.beta.matrix.for.SLMA
  ses <- mod$output.summary$input.se.matrix.for.SLMA
  variables <- rownames(betas)
  names <- names(mod$output.summary)[1:mod$num.valid.studies]
  
  if(is.null(variable)){ # If variable is NULL, use first variable that is not the intercept
    if(variables[1] == "(Intercept)"){variable <- variables[2]} else {variable <- variables[1]}
  } else { # Check supplied variable exists
    if(!(variable %in% variables)){
      stop("[", variable, "] is not valid. Valid variables [", paste(variables, collapse = ", "), "]")
    }
  }
  
  # Perform meta-analysis using the meta package
  res <- meta::metagen(TE = betas[variable,], seTE = ses[variable,], studlab = names, method.tau = method)
  
  # Plot the resulting forestplot using the meta package
  meta::forest(res, layout = layout)
  
}
