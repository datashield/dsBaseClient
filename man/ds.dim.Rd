\name{ds.dim}
\alias{ds.dim}
\title{Retrieves the dimension of an object}
\usage{
ds.dim(datasources = NULL, x = NULL, type = "split")
}
\arguments{
  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources.}

  \item{x}{a character, the name of R table object, for
  example a matrix, array or data frame}

  \item{type}{a character which represents the type of
  analysis to carry out. If \code{type} is set to
  'combine', a global variance is calculated}
}
\value{
for an array, \code{NULL} or a vector of mode
\code{integer}
}
\description{
this function is similar to R function \code{dim}
}
\details{


the function returns the polled dimension of the object by
summing up the individuals dimensions returned from each
study. Unlike the other DataSHIELD function the default
behaviour is to output the dimension of each study
separately.
}
\examples{
{

# load that contains the login details
data(logindata)

# login
opals <- datashield.login(logins=logindata,assign=TRUE)

# Example 1: Get the dimension of the assigned datasets in each study
ds.dim(datasources=opals, x='D')

# Example 2: Get the pooled dimension of the assigned datasets
ds.dim(datasources=opals, x='D', type='combine')

# Example 2: Input has to be either matrix, data frame or an array
\dontrun{ ds.dim(datasources=opals, x='D$LAB_TSC') }
}
}
\author{
Gaye, A.; Isaeva, J.
}

