% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ds.Boole.R
\name{ds.Boole}
\alias{ds.Boole}
\title{Converts a server-side R object into Boolean indicators}
\usage{
ds.Boole(
  V1 = NULL,
  V2 = NULL,
  Boolean.operator = NULL,
  numeric.output = TRUE,
  na.assign = "NA",
  newobj = NULL,
  datasources = NULL
)
}
\arguments{
\item{V1}{A character string specifying the name of the vector to which the Boolean operator
is to be applied.}

\item{V2}{A character string specifying the name of the vector to compare with \code{V1}.}

\item{Boolean.operator}{A character string specifying one of six possible Boolean operators:
\code{'==', '!=', '>', '>=', '<'} and \code{'<='}.}

\item{numeric.output}{logical. If TRUE the output variable should be of class numeric (\code{1/0}).
If FALSE the output variable should be of class logical (\code{TRUE/FALSE}). 
Default TRUE.}

\item{na.assign}{A character string taking values \code{'NA'},\code{'1'} or \code{'0'}.
Default \code{'NA'}. For more information see details.}

\item{newobj}{a character string that provides the name for the output 
object that is stored on the data servers. Default \code{boole.newobj}.}

\item{datasources}{a list of \code{\link[DSI]{DSConnection-class}} 
objects obtained after login. If the \code{datasources} argument is not specified
the default set of connections will be used: see \code{\link[DSI]{datashield.connections_default}}.}
}
\value{
\code{ds.Boole} returns the object specified by the \code{newobj} argument 
which is written to the server-side. Also, two validity messages are returned
to the client-side indicating the name of the \code{newobj} which 
has been created in each data source and if 
it is in a valid form.
}
\description{
It compares R objects using the standard set of 
Boolean operators (\code{==, !=, >, >=, <, <=}) to create a
vector with Boolean indicators that can be of class logical (\code{TRUE/FALSE}) 
or numeric (\code{1/0}).
}
\details{
A combination of different Boolean operators using \code{AND} operator
can be obtained by multiplying two or more
binary/Boolean vectors together. In this way, observations taking the value 1 in every vector
will then take the value 1 in the final vector (after multiplication)
while all others will take the value 0. Instead the combination using  \code{OR} operator
can be obtained by the sum of two or more vectors and applying   
\code{ds.Boole} using the operator \code{>= 1}. 

In \code{na.assign} if \code{'NA'} is specified, the missing values 
remain as \code{NA}s in the output vector. If \code{'1'} or \code{'0'} is specified the 
missing values are converted to 1 or 0 respectively or \code{TRUE}
or \code{FALSE} depending on the argument \code{numeric.output}.



Server function called: \code{BooleDS}
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

  # Generating Boolean indicators
  ds.Boole(V1 = "D$LAB_TSC",
           V2 = "D$LAB_TRIG",
           Boolean.operator = ">",
           numeric.output = TRUE, #Output vector of 0 and 1
           na.assign = "NA",      
           newobj = "Boole.vec",
           datasources = connections[1]) #only the first server is used ("study1")
           
  ds.Boole(V1 = "D$LAB_TSC",
           V2 = "D$LAB_TRIG",
           Boolean.operator = "<",
           numeric.output = FALSE, #Output vector of TRUE and FALSE 
           na.assign = "1", #NA values are converted to TRUE
           newobj = "Boole.vec",
           datasources = connections[2]) #only the second server is used ("study2") 
                      
  ds.Boole(V1 = "D$LAB_TSC",
           V2 = "D$LAB_TRIG",
           Boolean.operator = ">",
           numeric.output = TRUE, #Output vector of 0 and 1
           na.assign = "0", #NA values are converted to 0      
           newobj = "Boole.vec",
           datasources = connections) #All servers are used
  
  # Clear the Datashield R sessions and logout           
  datashield.logout(connections)
}
 
}
\author{
DataSHIELD Development Team
}
