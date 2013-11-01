\name{ds.is.null}
\alias{ds.is.null}
\title{tests whether an object is NULL}
\usage{
  ds.is.null(datasources = NULL, xvect = NULL)
}
\arguments{
  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources.}

  \item{xvect}{a vector to be tested}
}
\value{
  a logic value (TRUE if xvect is null and FALSE otherwise)
}
\description{
  this function is similar to R function \code{is.null}
}
\examples{
{

# load that contains the login details
data(logindata)

# login
opals <- datashield.login(logins=logindata,assign=TRUE)

# Example 1: Test whether LAB_TSC variable is NULL
ds.is.null(datasources=opals, xvect=quote(D$LAB_TSC))

}
}
\author{
  Gaye, A. (amadou.gaye
}

