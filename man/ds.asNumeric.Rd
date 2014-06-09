\name{ds.asNumeric}
\alias{ds.asNumeric}
\title{Turns a vector into numerical type}
\usage{
ds.asNumeric(xvect = NULL, newobj = NULL, datasources = NULL)
}
\arguments{
  \item{xvect}{a character, the name of the input vector.}

  \item{newobj}{the name of the new variable. If this
  argument is set to NULL, the name of the new variable is
  the name of the input variable with the suffixe '_num'.}

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
This function is similar to R function \code{as.numeric}.
}
\details{


See details of the R function 'as.numeric'.
}
\examples{
{

# load that contains the login details
data(logindata)

# login and assign specific variable(s)
# (by default the assigned dataset is a dataframe named 'D')
myvar <- list("GENDER")
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# turn the factor variable 'GENDER' into a character vector
ds.asCharacter(xvect='D$GENDER', newobj="gender_ch")

# now turn the newly created vector 'gender_ch' into a numeric
ds.asNumeric(xvect='gender_ch', newobj="gender_nm")
}
}
\author{
Gaye, A.
}

