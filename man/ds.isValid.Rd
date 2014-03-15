\name{ds.isValid}
\alias{ds.isValid}
\title{Checks if an variable is valid}
\usage{
ds.isValid(datasources = NULL, xvect = NULL)
}
\arguments{
  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources.}

  \item{xvect}{a numeric or factor vector.}
}
\value{
a list with booleans (one for each study), TRUE if input
vector is valid and FALSE otherwise.
}
\description{

}
\details{
This function checks if an input variable is valid. In
DataSHIELD, a numeric variable is not valid if it has > 0
and < 5 observations and a factor variable is not valid if
any of its levels (categories) has a count of between 1 and
4. The process is stopped if the input variable is not
valid to prevent further processing that might be
revealing.
}
\examples{
{

# load that contains the login details
data(logindata)

# login and assign specific variable(s)
myvar <- list("LAB_TSC", "GENDER")
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# Example 1: Check if the numerical variable 'LAB_TSC' is valid.
ds.isValid(datasources=opals, xvect=quote(D$LAB_TSC))

# Example 2: Check if the factor variable 'GENDER' is valid.
ds.isValid(datasources=opals, xvect=quote(D$GENDER))

}
}
\author{
Gaye, A.
}

