\name{ds.var}
\alias{ds.var}
\title{Computes the variance of a given vector (for several studies separately or combined)}
\usage{
ds.var(datasources = NULL, xvect = NULL, type = "combine")
}
\arguments{
  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources.}

  \item{xvect}{a string character, the name of a numerical
  vector}

  \item{type}{a character which represents the type of
  analysis to carry out. If \code{type} is set to
  'combine', a global variance is calculated if \code{type}
  is set to 'split', the variance is calculated separately
  for each study.}
}
\value{
a a global variance or one variance for each study.
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

# Example 1: compute the pooled variance of the variable 'LAB_TSC' - default behaviour
<<<<<<< HEAD
ds.var(datasources=opals, xvect="D$LAB_TSC")

# Example 2: compute the variance of each study separately
ds.var(datasources=opals, xvect="D$LAB_TSC", type="split")
=======
ds.var(datasources=opals, xvect='D$LAB_TSC')

# Example 2: compute the variance of each study separately
ds.var(datasources=opals, xvect='D$LAB_TSC', type='split')
>>>>>>> agaye
}
}
\author{
Gaye, A.
}

