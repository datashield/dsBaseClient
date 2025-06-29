% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ds.scatterPlot.R
\name{ds.scatterPlot}
\alias{ds.scatterPlot}
\title{Generates non-disclosive scatter plots}
\usage{
ds.scatterPlot(
  x = NULL,
  y = NULL,
  method = "deterministic",
  k = 3,
  noise = 0.25,
  type = "split",
  return.coords = FALSE,
  datasources = NULL
)
}
\arguments{
\item{x}{a character string specifying the name of the explanatory variable, a numeric vector.}

\item{y}{a character string specifying the name of the response variable,  a numeric vector.}

\item{method}{a character string that specifies the 
method that is used to generated non-disclosive
coordinates to be displayed in a scatter plot. 
This argument can be set as \code{'deteministic'} or \code{'probabilistic'}.
Default \code{'deteministic'}. 
For more information see \strong{Details}.}

\item{k}{the number of the nearest neighbors  for which their centroid is calculated.
Default 3. 
For more information see \strong{Details}.}

\item{noise}{the percentage of the initial variance that is used as the variance of the embedded
noise if the argument \code{method} is set to \code{'probabilistic'}.
For more information see \strong{Details}.}

\item{type}{a character that represents the type of graph to display.
This can be set as \code{'combine'} or \code{'split'}. 
Default \code{'split'}. 
For more information see \strong{Details}.}

\item{return.coords}{a logical. If TRUE the coordinates of the anonymised data points are return 
to the Console. Default value is FALSE.}

\item{datasources}{a list of \code{\link[DSI]{DSConnection-class}} objects obtained after login. 
If the \code{datasources} argument is not specified
the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.}
}
\value{
\code{ds.scatterPlot} returns to the client-side one or more scatter 
plots depending on the argument \code{type}.
}
\description{
This function uses two disclosure control methods to generate non-disclosive
scatter plots of two server-side continuous variables.
}
\details{
As the generation of a scatter plot from original data is disclosive and is not
permitted in DataSHIELD, this function allows the user to plot non-disclosive scatter plots.

If the argument \code{method} is set to \code{'deterministic'}, the server-side function searches
for the \code{k-1} nearest neighbors of each single data point and calculates the centroid 
of such \code{k} points. 
The proximity is defined by the minimum Euclidean distances of z-score transformed data.

When the coordinates of all centroids are estimated the function applies scaling to expand the
centroids back to the dispersion of the original data. The scaling is achieved by multiplying
the centroids with a scaling factor that is equal to the ratio between the standard deviation of
the original variable and the standard deviation of the calculated centroids. The coordinates of
the scaled centroids are then returned to the client-side.

The value of \code{k} is specified by the user. 
The suggested and default value is equal to 3 which is also
the suggested minimum threshold that is used to prevent disclosure which is specified in the
protection filter \code{nfilter.kNN}. When the value of \code{k} increases, 
the disclosure risk decreases but the utility loss increases.
The value of \code{k} is used only
if the argument \code{method} is set to \code{'deterministic'}. 
Any value of \code{k} is ignored if the
argument \code{method} is set to \code{'probabilistic'}.

If the argument \code{method} is set to \code{'probabilistic'}, 
the server-side function generates a random normal noise of zero mean
and variance equal to 10\% of the variance of each \code{x} and \code{y} variable.
The noise is added to each \code{x} and \code{y} variable and the disturbed by the addition of
\code{noise} data are returned to the client-side. Note that the seed random number generator is fixed to a
specific number generated from the data and therefore the user gets the same figure every time
that chooses the probabilistic method in a given set of variables.
The value of \code{noise} is used only if the argument \code{method} is set to \code{'probabilistic'}.
Any value of \code{noise} is ignored if
the argument \code{method} is set to \code{'deterministic'}. 

In \code{type} argument can be set two graphics to display:\cr
(1) If \code{type = 'combine'}  a scatter plot for
combined data is generated.\cr
(2) If \code{type = 'split'}  one scatter plot for each
study is generated. 

Server function called: \code{scatterPlotDS}
}
\examples{
\dontrun{

  ## Version 6, for version 5 see the Wiki 
  # Connecting to the Opal servers

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
  # Log onto the remote Opal training servers
  connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D") 

  #Example 1: generate a scatter plot for each study separately
  #Using the default deterministic method and k = 10
  
  ds.scatterPlot(x = "D$PM_BMI_CONTINUOUS",
                 y = "D$LAB_GLUC_ADJUSTED",
                 method = "deterministic",
                 k = 10,
                 type = "split",
                 datasources = connections)

  #Example 2: generate a combined scatter plot with the probabilistic method
  #and noise of variance 0.5\% of the variable's variance, and display the coordinates
  # of the anonymised data points to the Console
  
  ds.scatterPlot(x = "D$PM_BMI_CONTINUOUS",
                 y = "D$LAB_GLUC_ADJUSTED",
                 method = "probabilistic",
                 noise = 0.5,
                 type = "combine",
                 datasources = connections)
                   
  #Clear the Datashield R sessions and logout
  datashield.logout(connections) 

}

}
\author{
DataSHIELD Development Team
}
