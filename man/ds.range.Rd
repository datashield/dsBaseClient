\name{ds.range}
\alias{ds.range}
\title{Gets the range of a vector}
\usage{
  ds.range(datasources = NULL, xvect = NULL,
    type = "combine")
}
\arguments{
  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources.}

  \item{xvect}{a vector}

  \item{type}{a character which represents the type of
  analysis to carry out. If \code{type} is set to
  'combine', a global range is calculated if \code{type} is
  set to 'split', the range is calculated separately for
  each study.}
}
\value{
  a numeric vector which contains the minimum and the
  maximum
}
\description{
  this function is similar to R function \code{range} but
  instead to not return the real minimum and maximum, the
  computed values are multiplied by a very small random
  number. In addition it also generates a global range
  (i.e. the range of a vector across several datasets)
}
\examples{
{

# load that contains the login details
data(logindata)

# login and assign specific variable(s)
myvar <- list("LAB_TSC")
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# Example 1: Get the global range of the variable 'LAB_TSC' - default behaviour
ds.range(datasources=opals, xvect=quote(D$LAB_TSC))

# Example 2: Get the range of the variable 'LAB_TSC' for each study
ds.range(datasources=opals, xvect=quote(D$LAB_TSC), type="split")
}
}
\author{
  Gaye, A. (amadou.gaye
}

