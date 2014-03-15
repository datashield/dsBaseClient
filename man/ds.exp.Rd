\name{ds.exp}
\alias{ds.exp}
\title{Computes the exponential function}
\usage{
ds.exp(datasources = NULL, xvect = NULL, newobj = NULL)
}
\arguments{
  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources.}

  \item{xvect}{a vector.}

  \item{newobj}{the name of the new vector.If this argument
  is set to \code{NULL}, the name of the new variable is
  the name of the input variable with the suffixe '_exp'
  (e.g. 'PM_BMI_CONTINUOUS_exp', if input variable's name
  is 'PM_BMI_CONTINUOUS')}
}
\value{
a message is displayed when the action is completed.
}
\description{
This function is similar to R function \code{exp}.
}
\details{

}
\examples{
{

# load that contains the login details
data(logindata)

# login and assign specific variable(s)
myvar <- list("PM_BMI_CONTINUOUS")
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# compute exponential function of the 'PM_BMI_CONTINUOUS' variable
ds.exp(datasources=opals, xvect=quote(D$PM_BMI_CONTINUOUS))

}
}
\author{
Gaye, A.; Isaeva, J.
}

