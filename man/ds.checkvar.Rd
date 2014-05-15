\name{ds.checkvar}
\alias{ds.checkvar}
\title{Checks if all variables do exist and are not empty}
\usage{
ds.checkvar(datasources = NULL, variables = NULL)
}
\arguments{
  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources.}

  \item{variables}{a character vector, the names of the
  variable(s) to check}
}
\value{
the opal objects which passed the checks
}
\description{
This function checks that the variables to analyse are (1)
available from all the studies and (2) that they do not
contain only missing values (NAs). It excludes studies that
fail any of these two checks.
}
\details{


if this function is given a variable(s) created/derived
from a variable(s) initially assigned using through
'datashield.login' only the second check is carried out as
it is not then necessary to check is the variable is
available.
}
\examples{
{

# load that contains the login details
data(logindata)

# login and assign specific variable(s)
myvar <- list("LAB_TSC", "LAB_HDL")
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# Example 1: run checks for the variable LAB_TSC assigned above
ds.checkvar(datasources=opals, variables=list(quote(D$LAB_TSC)))

# Example 2: lets assign LAB_TSC to another variable 'tsc' not attached to 'D' and check 'tsc'
datashield.assign(opals, 'tsc', quote(D$LAB_TSC))
ds.checkvar(datasources=opals, variables=list(quote(tsc)))

}
}
\author{
Gaye, A.
}

