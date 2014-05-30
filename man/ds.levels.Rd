\name{ds.levels}
\alias{ds.levels}
\title{Provides access to the levels attribute of a factor variable}
\usage{
ds.levels(xvect = NULL, datasources = NULL)
}
\arguments{
  \item{xvect}{a character, the name of a factor variable}

  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources.}
}
\value{
levels of xvect
}
\description{
This function is similar to R function \code{levels}
}
\details{


this is just a wrapper function ofr the server side
function.
}
\examples{
{

# load that contains the login details
data(logindata)

# login
opals <- datashield.login(logins=logindata,assign=TRUE)

# Example 1: Get the levels of the PM_BMI_CATEGORICAL variable
ds.levels(xvect='D$PM_BMI_CATEGORICAL')

# Example 2: Get the levels of the LAB_TSC   SHOULD NOT WORK AS IT IS A CONTINUOUS VARIABLE
\dontrun{ ds.levels(xvect='D$LAB_TSC') }
}
}
\author{
Gaye, A.; Isaeva, J.
}

