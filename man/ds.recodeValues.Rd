% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ds.recodeValues.R
\name{ds.recodeValues}
\alias{ds.recodeValues}
\title{Recodes server-side variable values}
\usage{
ds.recodeValues(
  var.name = NULL,
  values2replace.vector = NULL,
  new.values.vector = NULL,
  missing = NULL,
  newobj = NULL,
  datasources = NULL,
  notify.of.progress = FALSE
)
}
\arguments{
\item{var.name}{a character string providing the name of the variable to be recoded.}

\item{values2replace.vector}{a numeric or character vector specifying the values
in the variable \code{var.name} to be replaced.}

\item{new.values.vector}{a numeric or character vector specifying the new values.}

\item{missing}{If supplied, any missing values in var.name will be replaced by this value. 
Must be of length 1. If the analyst want to recode only missing values then it should also 
specify an identical vector of values in both arguments \code{values2replace.vector} and 
\code{new.values.vector}. Otherwise please look the \code{ds.replaceNA} function.}

\item{newobj}{a character string that provides the name for the output object
that is stored on the data servers.
Default \code{recodevalues.newobj}.}

\item{datasources}{a list of \code{\link[DSI]{DSConnection-class}} 
objects obtained after login. If the \code{datasources} argument is not specified
the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.}

\item{notify.of.progress}{logical. If TRUE console output should be produced to indicate
progress. Default FALSE.}
}
\value{
Assigns to each server a new variable with the recoded values. 
Also, two validity messages are returned to the client-side 
indicating whether the new object has been created in each data source and if so whether
it is in a valid form.
}
\description{
This function takes specified values of elements in a vector and converts
them to a matched set of alternative specified values.
}
\details{
This function recodes individual values with new individual values. This can
apply to numeric and character values, factor levels and NAs. One particular use of
\code{ds.recodeValues} is to convert NAs to an explicit value. This value is specified
in the argument \code{missing}. If tthe user want to recode only missing values, then it
should also specify an identical vector of values in both arguments \code{values2replace.vector}
and \code{new.values.vector} (see Example 2 below).
Server function called: \code{recodeValuesDS}
}
\examples{
\dontrun{

  ## Version 6, for version 5 see the Wiki
  
  # connecting to the Opal servers

  require('DSI')
  require('DSOpal')
  require('dsBaseClient')

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
  
  connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D") 
  
  # Example 1: recode the levels of D$GENDER
  ds.recodeValues(var.name = "D$GENDER", 
                  values2replace.vector = c(0,1), 
                  new.values.vector = c(10,20),
                  newobj = 'gender_recoded',
                  datasources = connections)
                  
  # Example 2: recode NAs in D$PM_BMI_CATEGORICAL          
  ds.recodeValues(var.name = "D$PM_BMI_CATEGORICAL", 
                  values2replace.vector = c(1,2), 
                  new.values.vector = c(1,2),
                  missing = 99, 
                  newobj = 'bmi_recoded'
                  datasources = connections)
                 
  # Clear the Datashield R sessions and logout                 
  datashield.logout(connections) 
  
}   
}
\author{
DataSHIELD Development Team
}
