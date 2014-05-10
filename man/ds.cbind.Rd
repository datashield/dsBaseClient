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

  \item{objects}{a list which holds the names of the
  objects to combine}

  \item{newobj}{the name of the output object. If this
  argument is set to \code{NULL}, the name of the new
  object is 'newObject'.}
}
\value{
a message is displayed when the action is completed.
}
\description{
this is similar to the R base function 'cbind'
}
\details{


the function combines vectors or vectors and matrices or
dataframes by columns. But unlike the R base function
'cbind' the output is a dataframe.
}
\examples{
{

# load the file that contains the login details
data(logindata)

# login and assign the required variables to R
myvar <- list("LAB_TSC","LAB_HDL")
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# generate a new dataframe by combining the variables 'LAB_TSC' and 'LAB_HDL', by columns
ds.assign(opals, 'labtsc', 'D$LAB_TSC')
ds.assign(opals, 'labhdl', 'D$LAB_HDL')
myobjects <- list('labtsc', 'labhdl')
ds.cbind(datasources=opals, objects=myobjects)

}
}
\author{
Gaye, A.
}

