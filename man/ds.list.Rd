\name{ds.list}
\alias{ds.list}
\title{Function to construct a list object}
\usage{
  ds.list(datasources = NULL, vector = NULL, newobj = NULL)
}
\arguments{
  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources.}

  \item{vector}{a vector that holds the objects to contruct
  into a list}

  \item{newobj}{the name of the output object. If this
  argument is set to \code{NULL}, the name of the new
  object is 'list_vect'.}
}
\value{
  a message is displayed when the action is completed.
}
\description{
  Function to construct a list object
}
\examples{
{

# load the file that contains the login details
data(logindata)

# login and assign the required variables to R
myvar <- list("LAB_TSC","LAB_HDL")
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# combine the 'LAB_TSC' and 'LAB_HDL' variables into a list
myvect <- quote(list(D$LAB_TSC, D$LAB_HDL))
ds.list(datasources=opals, vector=myvect)
}
}
\author{
  Gaye, A.; Isaeva, J.
}

