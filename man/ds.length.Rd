\name{ds.length}
\alias{ds.length}
\title{Gets the length of a vector}
\usage{
ds.length(datasources = NULL, xvect = NULL, type = "combine")
}
\arguments{
  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources.}

  \item{xvect}{a string character, the name of a vector}

  \item{type}{a character which represents the type of
  analysis to carry out. If \code{type} is set to
  'combine', a global variance is calculated if \code{type}
  is set to 'split', the variance is calculated separately
  for each study.}
}
\value{
a numerical value, the number of elements of the input
vector
}
\description{
this function is similar to R function \code{length} with
the addition that it also generates a global length (i.e.
of the lengths of a vector across several datasets)
}
\details{

}
\examples{
{

# load that contains the login details
data(logindata)

# login and assign specific variable(s)
myvar <- list("LAB_TSC")
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# Example 1: Get the total number of observations across all the studies for the variable 'LAB_TSC' - default behaviour
<<<<<<< HEAD
ds.length(datasources=opals, xvect="D$LAB_TSC")

# Example 2: Get the number of observations on each study, for the variable 'LAB_TSC'
ds.length(datasources=opals, xvect="D$LAB_TSC", type="split")
=======
ds.length(datasources=opals, xvect='D$LAB_TSC')

# Example 2: Get the number of observations on each study, for the variable 'LAB_TSC'
ds.length(datasources=opals, xvect='D$LAB_TSC', type='split')
>>>>>>> agaye
}
}
\author{
Gaye, A.
}

