\name{ds.dataframe}
\alias{ds.dataframe}
\title{Creates data frames}
\usage{
ds.dataframe(x = NULL, newobj = NULL, row.names = NULL,
  check.rows = FALSE, check.names = TRUE, stringsAsFactors = TRUE,
  datasources = NULL)
}
\arguments{
  \item{x}{a character vector which contains the name(s) of
  the vector(s) to combine.}

  \item{row.names}{NULL or a character vector specifying
  the names of the rows.}

  \item{check.rows}{logical, if TRUE then the rows are
  checked for consistency of length and names.}

  \item{check.names}{logical, logical. If TRUE then the
  names of the variables in the data frame are checked to
  ensure that they are syntactically valid variable names
  and are not duplicated.}

  \item{stringsAsFactors}{logical, tells if character
  vectors should be converted to factors?}

  \item{newobj}{the name of the output object. If this
  argument is set to \code{NULL}, the name of the new
  object is 'dframe'.}

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
This is similar to the r function 'data.frame'.
}
\details{


Takes one or more vectors and generates a data frame
structure. # if the sought data frame is not valid (i.e.
contains a number of rows less than the minimum allowed
number of observation in DataSHIELD), an empty data frame
is created (i.e. a data frame that holds missing values
only - NA).
}
\examples{
{

# load the file that contains the login details
data(logindata)

# login and assign the required variables to R
myvar <- list('LAB_TSC','LAB_HDL')
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# create a dataframe that contains the variables the 'LAB_TSC' and 'LAB_HDL'
# all the arguments are set to default in this example
myvectors <- c('D$LAB_TSC', 'D$LAB_HDL')
ds.dataframe(x=myvectors)

# clear the Datashield R sessions and logout
datashield.logout(opals)

}
}
\author{
Gaye, A.; Isaeva, J.
}

