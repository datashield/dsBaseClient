\name{ds.createfactor}
\alias{ds.createfactor}
\title{create and combines factor vectors.}
\usage{
  ds.createfactor(opals = NULL, xvect = NULL,
    newvarname = NULL)
}
\arguments{
  \item{opals}{a list of opal object(s) obtained after
  login in to opal servers; these objects hold also the
  data assign to R, as \code{dataframe}, from opal
  datasources.}

  \item{xvect}{a numeric of character vector.}

  \item{newvarname}{name of the variable to assigned the
  created factor to.}
}
\value{
  a list of \code{factor} vectors or one factor vector.
}
\description{
  This function calls a server function that generates
  \code{factor} vectors. If the generated factor is not
  valid (i.e. if any of the categories has a count > 0 and
  < 5) its values are replaced by missing values.
}
\examples{
{

# load the login data
data(logindata)

# login and assign specific variable(s)
myvar <- list("GENDER")
opals <- ds.login(logins=logindata,assign=TRUE,variables=myvar)

# create a factor vector of the variable 'GENDER', one for each study
ds.createfactor(opals=opals, "sex", xvect=quote(D$GENDER))
}
}
\author{
  Gaye, A.
}

