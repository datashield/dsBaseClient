\name{ds.asFactor}
\alias{ds.asFactor}
\title{Turns a numeric vector into factor type}
\usage{
ds.asFactor(xvect = NULL, newobj = NULL, datasources = NULL)
}
\arguments{
  \item{xvect}{a character, the name of numeric, integer or
  character vector}

  \item{newobj}{the name of the new vector.If this argument
  is set to NULL, the name of the new variable is the name
  of the input variable with the suffixe '_fact'.}

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
This function is similar to R function \code{as.factor} but
it does not allow users to create factors where a categorie
has less than two observations.
}
\details{


Unlike the R function 'as.factor' vectors where some levels
have less than 2 observations are not allow, an empty
vector (i.e. all values within it are set to NA) is then
generated.
}
\examples{
{

# load that contains the login details
data(logindata)

# login and assign specific variable(s)
myvar <- list('GENDER', 'LAB_HDL')
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# turn the factor variable 'GENDER' into numeric and then back into factor
ds.asNumeric(xvect='D$GENDER', newobj='gender_num')
ds.asFactor(xvect='gender_num', newobj='gender_fact')

# Now try to turn into a factor a numeric variable where some levels have less
# than 2 observations (as you would expect for a continuous variable)
# this will generate an 'empty' vector (i.e. all values within it are set to NA).
ds.asFactor(xvect='D$LAB_HDL', newobj='lab.hdl.fact')
# let us check the levels of the new vector
ds.levels('lab.hdl.fact')

}
}
\author{
Gaye, A.; Burton, P.
}

