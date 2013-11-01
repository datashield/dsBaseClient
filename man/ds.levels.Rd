\name{ds.levels}
\alias{ds.levels}
\title{Provides access to the levels attribute of a variable}
\usage{
  ds.levels(datasources = NULL, xvect = NULL)
}
\arguments{
  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources.}

  \item{xvect}{an R object (factor)}
}
\value{
  levels of xvect
}
\description{
  this function is similar to R function \code{levels}
}
\examples{
{

# load that contains the login details
data(logindata)

# login
opals <- datashield.login(logins=logindata,assign=TRUE)

# Example 1: Get the levels of the PM_BMI_CATEGORICAL variable
ds.levels(datasources=opals, xvect=quote(D$PM_BMI_CATEGORICAL))

# Example 2: Get the levels of the LAB_TSC   SHOULD NOT WORK AS IT IS A CONTINUOUS VARIABLE
ds.levels(datasources=opals, xvect=quote(D$LAB_TSC))
}
}
\author{
  Gaye, A. (amadou.gaye
}

