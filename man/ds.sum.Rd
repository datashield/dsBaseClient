\name{ds.sum}
\alias{ds.sum}
\title{Returns the sum of all the values present in its arguments}
\usage{
  ds.sum(datasources = NULL, vector = NULL, newobj = NULL)
}
\arguments{
  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources.}

  \item{vector}{a vector that holds the objects to sum
  (numeric, complex og logical objects)}

  \item{newobj}{the name of the output object. If this
  argument is set to \code{NULL}, the name of the new
  object is 'sum_vect'.}
}
\value{
  a message is displayed when the action is completed.
}
\description{
  Returns the sum of all the values present in its
  arguments
}
\examples{
{

# load the file that contains the login details
data(logindata)

# login and assign the required variables to R
myvar <- list("LAB_TSC","LAB_HDL")
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# compute the sum of 'LAB_TSC' and 'LAB_HDL' (removing missing values if exist)
myvect <- quote(sum(D$LAB_TSC, D$LAB_HDL))
ds.sum(datasources=opals, vector=myvect)
}
}
\author{
  Gaye, A. (amadou.gaye
}

