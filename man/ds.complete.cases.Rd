\name{ds.complete.cases}
\alias{ds.complete.cases}
\title{Returns a logical vector indicating which cases are complete, i.e., have no missing values.}
\usage{
ds.complete.cases(datasources = NULL, x = NULL, newobj = NULL)
}
\arguments{
  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources.}

  \item{x}{a list of objects to be checked for
  completeness.}

  \item{newobj}{the name of the new vector.If this argument
  is set to \code{NULL}, the name of the new variable is
  the name of the input variable with the suffixe
  '_complete' (e.g. 'D_complete', if input variable's name
  is 'D')}
}
\value{
a message is displayed when the action is completed.
}
\description{
This function is similar to R function
\code{complete.cases}.
}
\details{

}
\examples{
{

# load that contains the login details
data(logindata)

# login and assign specific variable(s)
opals <- datashield.login(logins=logindata,assign=TRUE)

# Create a vector with indices for complete observations (rows)
ds.complete.cases(datasources=opals, x=quote(D))

# Create a vector with indices for complete observations for LAB_TSC and PM_BMI_CONTINUOUS variables
input = list(quote(D$LAB_TSC),quote(D$PM_BMI_CONTINUOUS))
ds.complete.cases(datasources=opals, x=input, newobj='TSC_BMI_complete')
}
}
\author{
Gaye, A.; Isaeva, J.
}

