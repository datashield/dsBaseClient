\name{ds.inform}
\alias{ds.inform}
\title{Tells user if an object is defined on the server side.}
\usage{
ds.inform(datasources = NULL, obj = NULL)
}
\arguments{
  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources.}

  \item{obj}{a string charcater, the name of the object to
  check for.}
}
\value{
a message
}
\description{
Because server side objects are not visible to users, tis
function gives some information.
}
\details{


the function checks if the specified object is defined
(i.e. exists) on the server side and prints a message to
inform the user.
}
\examples{
{

# load table that contains the login details
data(logindata)

# login and assign specific variable(s)
myvar <- list("LAB_TSC", "LAB_HDL")
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# get the log values of the variable 'LAB_TSC' in D (default name of the assigned table) and assign it to 'logTSC'
ds.assign(datasources=opals, newobj="logTSC", toAssign="log(D$LAB_TSC)")

# now check if the object you just assigned is really stored on the server side
ds.inform(datasources=opals, obj="logTSC")

}
}
\author{
Gaye, A.
}

