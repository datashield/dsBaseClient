\name{ds.NROW}
\alias{ds.NROW}
\title{Returns the number of rows present in x}
\usage{
  ds.NROW(datasources = NULL, x = NULL, type = "combine")
}
\arguments{
  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources.}

  \item{x}{a vector, array, matrix or data frame}

  \item{type}{a character which represents the type of
  analysis to carry out. If \code{type} is set to
  'combine', a global number of rows is calculated if
  \code{type} is set to 'split', the number of rows is
  calculated separately for each study.}
}
\value{
  an \code{interger} of length 1 or \code{NULL}
}
\description{
  this function is similar to R function \code{NROW} with
  the addition that it also generates a global number of
  rows (i.e.total number of entries across several
  datasets)
}
\examples{
{

# load that contains the login details
data(logindata)

# login and assign specific variable(s)
opals <- datashield.login(logins=logindata,assign=TRUE)

# Example 1: Get the total number of observations across all the studies
ds.NROW(datasources=opals, x=quote(D))

# Example 2: Get the number of observations on each study
ds.NROW(datasources=opals, x=quote(D), type="split")

# Example 3: Get the total number of observations for the variable 'LAB_TSC' (should be the same as in Example 1)
ds.NROW(datasources=opals, x=quote(D$LAB_TSC))

}
}
\author{
  Gaye, A.; Isaeva, J.
}

