\name{ds.asNull}
\alias{ds.asNull}
\title{Ignores its argument and returns the value \code{NULL}.}
\usage{
ds.asNull(datasources = NULL, x = NULL, newobj = NULL)
}
\arguments{
  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources.}

  \item{x}{an object to be turned into \code{NULL}}

  \item{newobj}{the name of the new vector.If this argument
  is set to \code{NULL}, the name of the new variable is
  the name of the input variable with the suffixe '_null'
  (e.g. 'LAB_TSC_matrix', if input variable's name is
  'LAB_TSC')}
}
\value{
a message is displayed when the action is completed.
}
\description{
This function is similar to R function \code{as.null}.
}
\details{

}
\examples{
{

# load that contains the login details
data(logindata)

# login and assign specific variable(s)
opals <- datashield.login(logins=logindata,assign=TRUE)

# turn the LAB_TSC variable into \\code{NULL}
ds.asNull(datasources=opals, x=quote(D$LAB_TSC))
}
}
\author{
Gaye, A.; Isaeva, J.
}

