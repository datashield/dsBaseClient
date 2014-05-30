\name{ds.class}
\alias{ds.class}
\title{Retrieves the class of an object}
\usage{
ds.class(x = NULL, datasources = NULL)
}
\arguments{
  \item{x}{an R object}

  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources.}
}
\value{
a character the type of x
}
\description{
This function is similar to R function \code{class}
}
\details{

}
\examples{
{

# load that contains the login details
data(logindata)

# login
opals <- datashield.login(logins=logindata,assign=TRUE)

# Example 1: Get the class of the whole dataset
ds.class(x='D')

# Example 2: Get the class of the variable PM_BMI_CONTINUOUS
ds.class(x='D$LAB_TSC')
}
}
\author{
Gaye, A.; Isaeva, J.
}

