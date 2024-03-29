% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ds.forestplot.R
\name{ds.forestplot}
\alias{ds.forestplot}
\title{Forestplot for SLMA models}
\usage{
ds.forestplot(mod, variable = NULL, method = "ML", layout = "JAMA")
}
\arguments{
\item{mod}{\code{list} List outputed by any of the SLMA models of DataSHIELD (\code{ds.glmerSLMA}, 
\code{ds.glmSLMA}, \code{ds.lmerSLMA})}

\item{variable}{\code{character} (default \code{NULL}) Variable to meta-analyze and visualize, by setting this 
argument to \code{NULL} (default) the first independent variable will be used.}

\item{method}{\code{character} (Default \code{"ML"}) Method to estimate the between study variance. 
See details from \code{?meta::metagen} for the different options.}

\item{layout}{\code{character} (default \code{"JAMA"}) Layout of the plot. 
See details from \code{?meta::metagen} for the different options.}
}
\description{
Draws a foresplot of the coefficients for Study-Level Meta-Analysis performed with
DataSHIELD
}
\examples{
\dontrun{
  # Run a logistic regression
  
  builder <- DSI::newDSLoginBuilder()
  builder$append(server = "study1", 
                 url = "http://192.168.56.100:8080/", 
                 user = "administrator", password = "datashield_test&", 
                 table = "CNSIM.CNSIM1", driver = "OpalDriver")
  builder$append(server = "study2", 
                 url = "http://192.168.56.100:8080/", 
                 user = "administrator", password = "datashield_test&", 
                 table = "CNSIM.CNSIM2", driver = "OpalDriver")
  builder$append(server = "study3",
                 url = "http://192.168.56.100:8080/", 
                 user = "administrator", password = "datashield_test&", 
                 table = "CNSIM.CNSIM3", driver = "OpalDriver")
  logindata <- builder$build()
  
  # Log onto the remote Opal training servers
  connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D") 
  
  # Fit the logistic regression model

  mod <- ds.glmSLMA(formula = "DIS_DIAB~GENDER+PM_BMI_CONTINUOUS+LAB_HDL",
                data = "D",
                family = "binomial",
                datasources = connections)
                
  # Plot the results of the model
  ds.forestplot(mod)
}

}
