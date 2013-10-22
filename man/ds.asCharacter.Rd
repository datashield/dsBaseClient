\name{ds.asCharacter}
\alias{ds.asCharacter}
\title{Turns a vector into character type}
\usage{
  ds.asCharacter(datasources = NULL, xvect = NULL,
    newvect = NULL)
}
\arguments{
  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources.}

  \item{xvect}{a vector.}

  \item{newvect}{the name of the new vector.If this
  argument is set to NULL, the name of the new variable is
  the name of the input variable with the suffixe '_char'
  (e.g. 'GENDER_char', if input variable's name is
  'GENDER')}
}
\value{
  a message is displayed when the action is completed.
}
\description{
  This function is similar to R function
  \code{as.character}.
}
\examples{
{

# load that contains the login details
data(logindata)

# login and assign specific variable(s)
myvar <- list("GENDER")
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# turn the factor variable 'GENDER' into a character vector
ds.asCharacter(datasources=opals, xvect=quote(D$GENDER), newvect="gender_as_char")
}
}
\author{
  Gaye, A.
}

