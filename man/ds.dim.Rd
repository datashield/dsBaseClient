\name{ds.dim}
\alias{ds.dim}
\title{Retrieves the dimension of an object}
\usage{
  ds.dim(datasources = NULL, x = NULL)
}
\arguments{
  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources.}

  \item{x}{an R object, for example a matrix, array or data
  frame}
}
\value{
  for an array, \code{NULL} or a vector of mode
  \code{integer}
}
\description{
  this function is similar to R function \code{dim}
}
\examples{
{

# load that contains the login details
data(logindata)

# login
opals <- datashield.login(logins=logindata,assign=TRUE)

# Example 1: Get the dimension of the assigned datasets
ds.dim(datasources=opals, x=quote(D))

# Example 2: Input has to be either matrix, data frame or an array
ds.dim(datasources=opals, x=quote(D$LAB_TSC))
}
}
\author{
  Gaye, A. (amadou.gaye
}

