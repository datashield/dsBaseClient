\name{ds.log}
\alias{ds.log}
\title{Computes logarithms, by default natural logarithms}
\usage{
  ds.log(datasources = NULL, xvect = NULL, base = exp(1),
    newobj = NULL)
}
\arguments{
  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources.}

  \item{xvect}{a vector.}

  \item{base}{the base with respect to which logarithms are
  computed.}

  \item{newobj}{the name of the new variable. If this
  argument is set to NULL, the name of the new variable is
  the name of the input variable with the suffixe '_log'
  (e.g. 'LAB_TSC_log', if input variable's name is
  'LAB_TSC')}
}
\value{
  a message is displayed when the action is completed.
}
\description{
  This function is similar to R function \code{log}.
}
\examples{
{

# load that contains the login details
data(logindata)

# login and assign specific variable(s)
myvar <- list("LAB_TSC")
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# Compute natural logarithm of LAB_TSC
ds.log(datasources=opals, xvect=quote(D$LAB_TSC))

# Compute natural logarithm of LAB_TSC with base 2
ds.log(datasources=opals, xvect=quote(D$LAB_TSC), base=2)

}
}
\author{
  Gaye, A.; Isaeva, J.
}

