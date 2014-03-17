\name{ds.names}
\alias{ds.names}
\title{Gets the names of items in a list}
\usage{
ds.names(datasources = NULL, xlist = NULL)
}
\arguments{
  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources.}

  \item{xlist}{an object of type list}
}
\value{
a list which hold items names for each study
}
\description{

}
\details{
This function is similar to the R function \code{names} but
in DataSHIELD its use is restricted to objects of type
list.
}
\examples{
{

# load the login data
data(logindata)

# login and assign some variables to R
myvar <- list("DIS_DIAB","PM_BMI_CONTINUOUS","LAB_HDL", "GENDER")
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# generates subset tables from the table assigned above (by default the table name is 'D')
ds.subsetdata(datasources=opals, dataset="D")

# the above object 'subsets' is a list, let us display the names of elements in 'Subsets'
ds.names(opals, quote(subsets))

}
}
\author{
Gaye, A.
}

