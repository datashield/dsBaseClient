\name{ds.exist}
\alias{ds.exist}
\title{Checks if an object is defined on the server side}
\usage{
  ds.exist(datasources = NULL, xname = NULL)
}
\arguments{
  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources.}

  \item{xname}{the name of the object to look for.}
}
\value{
  a bollean, TRUE if the object is on the server side and
  FALSE otherwise
}
\description{
  In DataSHIELD it is not possible to see the data sitting
  on the servers of the collaborating studies. It is only
  possible to get summaries of objects stored on the server
  side. It is however important to know if an object
  created on the server is really there. This function
  checks if an object do really exist on the server side.
  Further information about the object can also be obtain
  using function such as \code{ds.class}, \code{length}
  etc...
}
\examples{
{

# load the file that contains the login details
data(logindata)

# login and assign the required variables to R
myvar <- list("LAB_TSC")
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# get the mean of the variable LAB_TSC for each study
stat.mean <- ds.mean(datasources=opals, xvect=quote(D$LAB_TSC))

# create a new variable, 'centered_labtsc', mean centered 'LAB_TSC'
datashield.assign(datasources=opals, "centered_labtsc", quote(D$LAB_TSC - stat.mean)

# now let us check if the variable do now 'exist' on the server side
ds.exists(datasources=opals, xname="centered_labtsc")
}
}
\author{
  Gaye, A. (amadou.gaye
}

