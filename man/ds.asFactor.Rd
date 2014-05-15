\name{ds.asFactor}
\alias{ds.asFactor}
\title{Turns a numeric vector into factor type}
\usage{
ds.asFactor(datasources = NULL, xvect = NULL, newobj = NULL)
}
\arguments{
  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources.}

  \item{xvect}{a numeric, integer or character vector}

  \item{newobj}{the name of the new vector.If this argument
  is set to NULL, the name of the new variable is the name
  of the input variable with the suffixe '_fact' (e.g.
  'GENDER_fact', if input variable's name is 'GENDER')}
}
\value{
a message is displayed when the action is completed.
}
\description{
This function is similar to R function \code{as.factor} but
it does not allow users to create factors where a categorie
has less than two observations.
}
\details{


if the input vector is of type numeric or integer it is
first turned into a character type and then only into a
factor.
}
\examples{
{

# load that contains the login details
data(logindata)

# login and assign specific variable(s)
myvar <- list('GENDER', 'LAB_HDL')
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# turn the factor variable 'GENDER' into numeric and then into factor
ds.asNumeric(datasources=opals, xvect='D$GENDER', newobj='gender_num')
ds.asFactor(datasources=opals, xvect='gender_num', newobj='gender_fact')

# Now try to turn into a factor a numeric variable where some levels with < 2 observations
# this will give return NULL because it is reveaaling
ds.asFactor(datasources=opals, xvect='D$LAB_HDL', newobj='lab.hdl.fact')
}
}
\author{
Gaye, A.; Burton, P.
}

