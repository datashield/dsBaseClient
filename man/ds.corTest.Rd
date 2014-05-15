\name{ds.corTest}
\alias{ds.corTest}
\title{Test for correlation between paired samples}
\usage{
ds.corTest(datasources = NULL, xvect = NULL, yvect = NULL)
}
\arguments{
  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources.}

  \item{xvect}{a character, the name of a numerical vector}

  \item{yvect}{a character, the name of a numerical vector}
}
\value{
a list containing the results of the test
}
\description{
This is similar to the R base function 'cor.test'.
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

# test for correlation between the variables 'LAB_TSC' and 'LAB_HDL'
ds.corTest(datasources=opals, xvect='D$LAB_TSC', yvect='D$LAB_HDL')
}
}
\author{
GAYE, A.; Burton, P.
}

