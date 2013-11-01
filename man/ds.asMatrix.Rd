\name{ds.asMatrix}
\alias{ds.asMatrix}
\title{Attempts to turn its argument into a matrix}
\usage{
  ds.asMatrix(datasources = NULL, x = NULL, newobj = NULL)
}
\arguments{
  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources.}

  \item{x}{an object to be converted into a matrix}

  \item{newobj}{the name of the new vector.If this argument
  is set to \code{NULL}, the name of the new variable is
  the name of the input variable with the suffixe '_matrix'
  (e.g. 'D_matrix', if input variable's name is 'D')}
}
\value{
  a message is displayed when the action is completed.
}
\description{
  This function is similar to R function \code{as.matrix}.
}
\examples{
{

# load that contains the login details
data(logindata)

# login and assign specific variable(s)
opals <- datashield.login(logins=logindata,assign=TRUE)

# turn the data frame D into a matrix
ds.asMatrix(datasources=opals, x=quote(D))
}
}
\author{
  Gaye, A. (amadou.gaye
}

