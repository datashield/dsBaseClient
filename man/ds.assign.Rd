\name{ds.assign}
\alias{ds.assign}
\title{Assigns an object to a name in the server side}
\usage{
ds.assign(datasources = NULL, newobj = "newObject", toAssign = NULL)
}
\arguments{
  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources.}

  \item{newobj}{the name of the new object}

  \item{toAssign}{a string character, the object to assign
  or the call to an assign function that generates the
  object to assign.}
}
\description{
This function assigns a datashield object to a name, hence
creating a new object. It also calls 'assign' server side
functions to generate objects stored on the server side.
The function is a wrapper for the 'opal' package function
'datashield.assign'.
}
\details{


The new object is stored on the local R instance (i.e. on
the server side). If no name is provided, the new object is
named 'newObject', by default.
}
\examples{
{

# load that contains the login details
data(logindata)

# login and assign specific variable(s)
myvar <- list("LAB_TSC")
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# Example 1: assign the variable 'LAB_TSC' in the dataframe D
ds.assign(datasources=opals, newobj="labtsc", toAssign="D$LAB_TSC")

# Example2: get the log values of the variable 'LAB_TSC' in D and assign it to 'logTSC'
ds.assign(datasources=opals, newobj="logTSC", toAssign="log(D$LAB_TSC)")
}
}
\author{
Gaye, A.
}

