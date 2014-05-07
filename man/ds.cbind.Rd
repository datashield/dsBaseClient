\name{ds.cbind}
\alias{ds.cbind}
\title{Combines objects by columns}
\usage{
ds.cbind(datasources = NULL, objects = NULL, newobj = NULL)
}
\arguments{
  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources.}

  \item{objects}{a list of objects to combine.}

  \item{newobj}{the name of the output object. If this
  argument is set to \code{NULL}, the name of the new
  object is 'newObject'.}
}
\value{
a message is displayed when the action is completed.
}
\description{

}
\examples{
{

# load the file that contains the login details
data(logindata)

# login and assign the required variables to R
myvar <- list("LAB_TSC","LAB_HDL")
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# combine the 'LAB_TSC' and 'LAB_HDL' variables by columns
myobjects <- list(D$LAB_TSC, D$LAB_HDL)
ds.cbind(datasources=opals, objects=myobjects)
}
}
\author{
Gaye, A.; Isaeva, J.
}

