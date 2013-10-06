\name{ds.isNA}
\alias{ds.isNA}
\title{Checks if a vector is empty}
\usage{
  ds.isNA(datasources = NULL, var2check = NULL)
}
\arguments{
  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal}

  \item{var2check}{a numerical or character vector}
}
\value{
  a boolean 'TRUE' if the vector contains on NAs and
  'FALSE' otherwise.
}
\description{
  this function is similar to R function \code{is.na} but
  instead of a vector of booleans it returns just one
  boolean to tell if all the elements are missing values.
}
\examples{
{
# load the login data
data(logindata)

# login and assign specific variable(s)
library(opal)
myvar <- list("LAB_HDL")
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# check if all the observation of the variable 'LAB_HDL' are missing (NA)
ds.isNA(datasources=opals, var2check=quote(D$LAB_HDL))
}
}
\author{
  Gaye, A.
}

