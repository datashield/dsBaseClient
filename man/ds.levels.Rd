\name{ds.levels}
\alias{ds.levels}
\title{Provides access to the levels attribute of a factor variable}
\usage{
ds.levels(datasources = NULL, xvect = NULL)
}
\arguments{
  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources.}

  \item{xvect}{a character, the name of a factor variable}
}
\value{
levels of xvect
}
\description{
This function is similar to R function \code{levels}
}
\details{

}
\examples{
{

# load that contains the login details
data(logindata)

# login
opals <- datashield.login(logins=logindata,assign=TRUE)

# Example 1: Get the levels of the PM_BMI_CATEGORICAL variable
ds.levels(datasources=opals, xvect='D$PM_BMI_CATEGORICAL')

# Example 2: Get the levels of the LAB_TSC   SHOULD NOT WORK AS IT IS A CONTINUOUS VARIABLE
\dontrun{ ds.levels(datasources=opals, xvect='D$LAB_TSC') }
}
}
\author{
Gaye, A.; Isaeva, J.
}

