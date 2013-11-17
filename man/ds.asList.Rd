\name{ds.asList}
\alias{ds.asList}
\title{Constructs an object of type list}
\usage{
  ds.asList(datasources = NULL, x = NULL, newobj = NULL)
}
\arguments{
  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources.}

  \item{x}{an object to be converted into a list}

  \item{newobj}{the name of the new vector.If this argument
  is set to NULL, the name of the new variable is the name
  of the input variable with the suffixe '_list' (e.g.
  'D_factor', if input variable's name is 'D')}
}
\value{
  a message is displayed when the action is completed.
}
\description{
  This function is similar to R function \code{as.list}.
}
\examples{
{

# load that contains the login details
data(logindata)

# login and assign specific variable(s)
opals <- datashield.login(logins=logindata,assign=TRUE)

# turn the data frame D into a list
ds.asList(datasources=opals, x=quote(D))
}
}
\author{
  Gaye, A.; Isaeva, J.
}

