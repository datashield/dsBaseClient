\name{ds.fac2num}
\alias{ds.fac2num}
\title{Turns a factor into numerical type}
\usage{
  ds.fac2num(datasources = NULL, xvect = NULL,
    newobj = NULL)
}
\arguments{
  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources.}

  \item{xvect}{a vector.}

  \item{newobj}{the name of the new variable. If this
  argument is set to NULL, the name of the new variable is
  the name of the input variable with the suffixe
  '_fac2num' (e.g. 'GENDER_fac2num', if input variable's
  name is 'GENDER)}
}
\value{
  a message is displayed when the action is completed.
}
\description{
  This function combines in itself two functions:
  \code{as.character} and \code{as.numeric} as in order to
  turn a factor into a numerical variable, one has to first
  turn it into a character.
}
\examples{
{

# load that contains the login details
data(logindata)

# login and assign specific variable(s)
myvar <- list("GENDER")
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# turn the factor variable 'GENDER' into a numeric vector
ds.fac2num(datasources=opals, xvect=quote(D$GENDER))

}
}
\author{
  Gaye, A.; Isaeva, I.
}

