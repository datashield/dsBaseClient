\name{ds.asCharacter}
\alias{ds.asCharacter}
\title{Turns a vector into character type vector}
\usage{
ds.asCharacter(xvect = NULL, newobj = NULL, datasources = NULL)
}
\arguments{
  \item{xvect}{a character, the name of the input vector.}

  \item{newobj}{the name of the new vector.If this argument
  is set to NULL, the name of the new variable is the name
  of the input variable with the suffixe '_char'.}

  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources. By default an internal function looks for
  'opal' objects in the environment and sets this
  parameter.}
}
\value{
nothing is returned to the client, the new object is stored
on the server side.
}
\description{
This function is similar to R function \code{as.character}.
}
\details{


See details of the R function 'as.character'.
}
\examples{
{

# load that contains the login details
data(logindata)

# login and assign specific variable(s)
myvar <- list("GENDER")
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# turn the factor variable 'GENDER' into a character vector
ds.asCharacter(xvect='D$GENDER', newobj="gender_char")

}
}
\author{
Gaye, A.
}

