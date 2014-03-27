\name{ds.mean}
\alias{ds.mean}
\title{Computes the statistical mean of a given vector (for several studies separately or combined)}
\usage{
ds.mean(datasources = NULL, xvect = NULL, type = "combine")
}
\arguments{
  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources.}

  \item{xvect}{a character, the name of a numerical vector}

  \item{type}{a character which represents the type of
  analysis to carry out. If \code{type} is set to
  'combine', a global mean is calculated if \code{type} is
  set to 'split', the mean is calculated separately for
  each study.}
}
\value{
a mean value
}
\description{

}
\examples{
{

# load that contains the login details
data(logindata)

# login and assign specific variable(s)
myvar <- list('LAB_TSC')
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# Example 1: compute the pooled statistical mean of the variable 'LAB_TSC' - default behaviour
ds.mean(datasources=opals, xvect='D$LAB_TSC')

# Example 2: compute the statistical mean of each study separately
ds.mean(datasources=opals, xvect='D$LAB_TSC', type='split')
}
}
\author{
Isaeva, J.
}

