\name{ds.cov}
\alias{ds.cov}
\title{Computes correlation and/or covariance values}
\usage{
ds.cov(datasources = NULL, xvect = NULL, yvect = NULL, type = "combine")
}
\arguments{
  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources.}

  \item{x}{a numeric vector matrix or dataframe}

  \item{y}{a numeric vector matrix or dataframe with
  compatible dimensions to x, set to NULL by default.}

  \item{type}{a character which represents the type of
  analysis to carry out. If \code{type} is set to
  'combine', a global mean is calculated if \code{type} is
  set to 'split', the mean is calculated separately for
  each study.}
}
\value{
variance, covariance or covariance matrix depending on the
input
}
\description{
This is similar to the R base function 'cov'.
}
\details{

}
\examples{
{

# load that contains the login details
data(logindata)

# login and assign specific variable(s)
myvar <- list('LAB_TSC', 'LAB_HDL')
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# compute
ds.cov(datasources=opals, xvect='D$LAB_TSC', yvect='D$LAB_HDL')
}
}
\author{
Gaye, A.; Burton, P.
}

