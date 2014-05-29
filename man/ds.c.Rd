\name{ds.c}
\alias{ds.c}
\title{Combines values into a vector or list}
\usage{
ds.c(vect = NULL, newobj = NULL, datasources = NULL)
}
\arguments{
  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources.}

  \item{vect}{a character vector that holds the names of
  the objects to combine.}

  \item{newobj}{the name of the output object. If this
  argument is set to NULL, the name of the new object is
  'cvect'.}
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
myvar <- c('LAB_TSC', 'LAB_HDL')
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# Get the variables 'LAB_TSC' by 'LAB_HDL' from the dataframe 'D' and combine them
myvect <- c('D$LAB_TSC', 'D$LAB_HDL')
ds.assign('labtsc', 'D$LAB_TSC')
ds.assign('labhdl', 'D$LAB_HDL')
myvect <- c('labtsc', 'labhdl')
ds.c(vect=myvect)
}
}
\author{
Gaye, A.
}

