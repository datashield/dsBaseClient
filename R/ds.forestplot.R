#' @title Forestplot for SLMA models
#' @description Draws a foresplot of the coefficients for Study-Level Meta-Analysis performed with
#' DataSHIELD
#'
#' @param mod \code{list} List outputed by any of the SLMA models of DataSHIELD (\code{ds.glmerSLMA}, 
#' \code{ds.glmSLMA}, \code{ds.lmerSLMA})
#' @param method \code{character} (Default \code{"ML"}) Parameter optimization method to visualize. Options are 
#' \code{"ML"} for Maximum Likelihood, \code{"REML"} for REstricted Maximum Likelihood or \code{"FE"} for 
#' Fixed-Effects meta-analysis 
#'
#' @return Plot object of class \code{vpPath}
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

ds.forestplot <- function(mod, method = "ML"){
  
  # Declaration of variables
  ntotal <- NULL
  nvalid <- NULL
  nmissing <- NULL
  names <- NULL
  
  # Get number of studies
  num_stud <- mod$num.valid.studies
  
  # Obtain from each study the numbers of valid, missing and total of individuals. Obtain the study names too
  for(i in 1:num_stud){
    ntotal <- c(ntotal, mod$output.summary[[i]]$Ntotal)
    nvalid <- c(nvalid, mod$output.summary[[i]]$Nvalid)
    nmissing <- c(nmissing, mod$output.summary[[i]]$Nmissing)
    names <- c(names, names(mod$output.summary)[i])
  }
  
  # Get t-test parameters
  n <- sum(nvalid)
  p <- nrow(mod$betamatrix.valid)
  
  # Get coefficients and calculate confidence interval using the standard errors
  mean <- t(mod$betamatrix.valid)
  lower <- t(mod$betamatrix.valid - mod$sematrix.valid * stats::qt(.975, n-p))
  upper <- t(mod$betamatrix.valid + mod$sematrix.valid * stats::qt(.975, n-p))
    
  # Format to be passed to the foresplot function
  names <- c("Study", names, NA, "Summary")
  ntotal <- c("N total", ntotal, NA, sum(ntotal))
  nvalid <- c("N valid", nvalid, NA, sum(nvalid))
  nmissing <- c("N missing", nmissing, NA, sum(nmissing))
  
  # Choice of ML, REML, FE. only would affect the columns chosen on next 3 lines
  if(method == "ML"){
    mean <- rbind(NA, mean, NA, mod$SLMA.pooled.ests.matrix[,1])
    lower <- rbind(NA, lower, NA, mod$SLMA.pooled.ests.matrix[,1] - mod$SLMA.pooled.ests.matrix[,2] * stats::qt(.975, n-p))
    upper <- rbind(NA, upper, NA, mod$SLMA.pooled.ests.matrix[,1] + mod$SLMA.pooled.ests.matrix[,2] * stats::qt(.975, n-p))
  }
  else if(method == "REML"){
    mean <- rbind(NA, mean, NA, mod$SLMA.pooled.ests.matrix[,3])
    lower <- rbind(NA, lower, NA, mod$SLMA.pooled.ests.matrix[,3] - mod$SLMA.pooled.ests.matrix[,4] * stats::qt(.975, n-p))
    upper <- rbind(NA, upper, NA, mod$SLMA.pooled.ests.matrix[,3] + mod$SLMA.pooled.ests.matrix[,4] * stats::qt(.975, n-p))
  }
  else if(method == "FE"){
    mean <- rbind(NA, mean, NA, mod$SLMA.pooled.ests.matrix[,5])
    lower <- rbind(NA, lower, NA, mod$SLMA.pooled.ests.matrix[,5] - mod$SLMA.pooled.ests.matrix[,6] * stats::qt(.975, n-p))
    upper <- rbind(NA, upper, NA, mod$SLMA.pooled.ests.matrix[,5] + mod$SLMA.pooled.ests.matrix[,6] * stats::qt(.975, n-p))
  }
  else{stop("Invalid 'method' argument [", method, "]")}
  
  # Build plot parameters
  legend <- colnames(mean)
  summary <- c(TRUE, rep(FALSE, num_stud),TRUE, TRUE)
  colors <- RColorBrewer::brewer.pal(n = length(legend), name = "Set2")
  tabletext <- cbind(names, ntotal, nvalid, nmissing)
  
  forestplot::forestplot(tabletext, mean = mean, lower = lower, upper = upper, legend = legend,
             is.summary = summary, col=forestplot::fpColors(box=colors,
                                                summary = colors),
             xlab="Coefficient")
  
}
