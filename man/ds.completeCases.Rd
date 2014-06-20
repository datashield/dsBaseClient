\name{ds.completeCases}
\alias{ds.completeCases}
\title{Find complete cases}
\usage{
ds.completeCases(x = NULL, newobj = NULL, datasources = NULL)
}
\arguments{
  \item{x}{a character, the name of a vector, dataframe or
  matrix.}

  \item{newobj}{the name of the new vector. If this
  argument is set to \code{NULL}, the name of the new
  variable is the name of the input variable with the
  suffixe '_complete'.}

  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources.}
}
\value{
nothing is returned to the client, the new object is stored
on the server side.
}
\description{
This function is similar to R function
\code{complete.cases}.
}
\details{


a logical vector indicating which cases are complete(i.e.
have no missing values) is generated and stored on the
server side. That object can then be tabulated using the
function 'ds.table1d' to find the extent of completeness.
}
\examples{
{

# load that contains the login details
data(logindata)

# login and assign all the stored variables
# (by default the assigned dataset is a datframe named 'D')
opals <- datashield.login(logins=logindata,assign=TRUE)

# Example 1: check completes cases on the dataframe 'D'
ds.completeCases(x='D')
# now tabulate the vector of comple cases
# remember default name is name of variable with suffix '_complete'
ds.table1d(x='D_complete')

# Example 2: check complte cases for the variable 'PM_BMI_CONTINUOUS'
ds.completeCases(x='D$PM_BMI_CONTINUOUS')
# now tabulate the vector of comple cases
ds.table1d(x='PM_BMI_CONTINUOUS_complete')

}
}
\author{
Gaye, A.; Isaeva, J.
}

