\name{ds.product}
\alias{ds.product}
\title{Computes a product}
\usage{
ds.product(datasources = NULL, xlist = NULL, newobj = NULL)
}
\arguments{
  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources.}

  \item{xlist}{a list of numerical to compute a product
  for.}

  \item{newobj}{the name of the product object. If this
  argument is set to NULL, the name of the new object is
  'prodOutput'.}
}
\value{
a message is displayed when the action is completed.
}
\description{

}
\details{
The function computes the product of up to five elements
}
\examples{
{

# load the file that contains the login details
data(logindata)

# login and assign the required variables to R
myvar <- list("LAB_TSC","LAB_HDL")
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# compute the product of 'LAB_TSC' by 'LAB_HDL' and assign it to 'P'
prodinput <- list(quote(D$LAB_TSC), quote(D$LAB_HDL))
ds.product(datasources=opals, xlist=prodinput, newobj=NULL)
}
}
\author{
Gaye, A.
}

