\name{ds.isPresent}
\alias{ds.isPresent}
\title{Checks if a vector is in a table assigned to R}
\usage{
ds.isPresent(datasources = NULL, var2check = NULL)
}
\arguments{
  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal.}

  \item{var2check}{a numerical or character vector}
}
\value{
a boolean 'TRUE' if the vector is present and 'FALSE'
otherwise.
}
\description{

}
\details{
the check is carried out only if the symbol of the table to
search in is provided - this is done by proving the second
argument in the form 'X$nameOfVariable' where 'X' is the
name of the table/dataframe assigned to R from opal.
}
\examples{
{
# load the login data
data(logindata)

# login and assign specific variable(s)
library(opal)
myvar <- list("LAB_HDL")
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

 # check if all entries of  variable 'LAB_HDL' hold missing values (NA)
 ds.isPresent(datasources=opals, var2check=quote(D$LAB_HDL))
}
}
\author{
Gaye, A.
}

