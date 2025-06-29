% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ds.quantileMean.R
\name{ds.quantileMean}
\alias{ds.quantileMean}
\title{Computes the quantiles of a server-side variable}
\usage{
ds.quantileMean(x = NULL, type = "combine", datasources = NULL)
}
\arguments{
\item{x}{a character string specifying the name of the numeric vector.}

\item{type}{a character that represents the type of graph to display.
This can be set as \code{'combine'} or \code{'split'}.
For more information see \strong{Details}.}

\item{datasources}{a list of \code{\link[DSI]{DSConnection-class}} 
objects obtained after login. If the \code{datasources} argument is not specified
the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.}
}
\value{
\code{ds.quantileMean} returns to the client-side the quantiles and statistical mean
of a server-side numeric vector.
}
\description{
This function calculates the mean and quantile values of a 
server-side quantitative variable.
}
\details{
This function does not return the minimum and maximum values
because they are potentially disclosive.

Depending on the argument \code{type} can be carried out two types of analysis: \cr
(1) \code{type = 'combine'} pooled values are displayed \cr
(2) \code{type = 'split'} summaries are
returned for each study. 

Server functions called: \code{quantileMeanDS}, \code{length} and \code{numNaDS}
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
  
  #Get the quantiles and mean of a server-side variable
  
  ds.quantileMean(x = "D$LAB_TRIG",
                  type = "combine",
                  datasources = connections)
  
  
  # clear the Datashield R sessions and logout
  datashield.logout(connections)


}

}
\seealso{
\code{\link{ds.mean}} to compute the statistical mean.

\code{\link{ds.summary}} to generate the summary of a variable.
}
\author{
DataSHIELD Development Team
}
