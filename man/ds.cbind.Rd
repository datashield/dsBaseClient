\name{ds.cbind}
\alias{ds.cbind}
\title{Combines objects by columns}
\usage{
ds.cbind(x = NULL, newobj = NULL, datasources = NULL)
}
\arguments{
  \item{x}{a vector which holds the names of the objects to
  combine}

  \item{newobj}{the name of the output object. If this
  argument is set to \code{NULL}, the name of the new
  object is 'newDataframe'.}

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

# login and assign specific variables(s)
# (by default the assigned dataset is a dataframe named 'D')
myvar <- list('LAB_TSC', 'LAB_HDL')
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# generate a new dataframe by combining the log values of 'LAB_TSC' and 'LAB_HDL', by columns
ds.assign(newobj='labtsc', toAssign='log(D$LAB_TSC)')
ds.assign(newobj='labhdl', toAssign='log(D$LAB_HDL)')
myobjects <- c('labtsc', 'labhdl')
ds.cbind(x=myobjects)

}
}
\author{
Gaye, A.
}

