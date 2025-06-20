% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ds.asMatrix.R
\name{ds.asMatrix}
\alias{ds.asMatrix}
\title{Converts a server-side  R object into a matrix}
\usage{
ds.asMatrix(x.name = NULL, newobj = NULL, datasources = NULL)
}
\arguments{
\item{x.name}{a character string providing the name of the input object to be coerced to 
a matrix.}

\item{newobj}{a character string that provides the name for the output object
that is stored on the data servers. Default \code{asmatrix.newobj}.}

\item{datasources}{a list of \code{\link[DSI]{DSConnection-class}} 
objects obtained after login. If the \code{datasources} argument is not specified
the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.}
}
\value{
\code{ds.asMatrix} returns the object converted into a matrix 
that is written to the server-side. Also, two validity messages are returned
to the client-side indicating the name of the \code{newobj} which 
has been created in each data source and if it is in a valid form.
}
\description{
Coerces an R object into a matrix. 
This converts all columns into character class.
}
\details{
This function is based on the native R function \code{as.matrix}. 
If this function is applied to a data frame, all columns are converted into a character class.
If you wish to convert a data frame to a matrix but maintain all data columns in their
original class you should use the function \code{ds.asDataMatrix}. 

Server function called: \code{asMatrixDS}
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
  
  # Converting the R object into a matrix
  ds.asMatrix(x.name = "D",
              newobj = "mat.obj",
              datasources = connections[1]) #only the first Opal server is used ("study1")
                 
  # Clear the Datashield R sessions and logout                 
  datashield.logout(connections) 
  
} 
}
\author{
DataSHIELD Development Team
}
