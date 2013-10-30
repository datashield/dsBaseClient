\name{ds.c}
\alias{ds.c}
\title{Combines values into a vector or list}
\usage{
  ds.c(datasources = NULL, vector = NULL, newobj = NULL)
}
\arguments{
  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources.}

  \item{vector}{a vector that holds the objects to combine}

  \item{newobj}{the name of the output object. If this
  argument is set to NULL, the name of the new object is
  'cvect'.}
}
\value{
  a message is displayed when the action is completed.
}
\description{
  Combines values into a vector or list
}
\examples{
{

# load the file that contains the login details
data(logindata)

# login and assign the required variables to R
myvar <- list("LAB_TSC","LAB_HDL")
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# compute the product of 'LAB_TSC' by 'LAB_HDL' and assign it to 'P'
myvect <- quote(c(D$LAB_TSC, D$LAB_HDL))
ds.c(datasources=opals, vector=myvect)
}
}
\author{
  Gaye, A. (amadou.gaye
}

