\name{ds.subsetdata}
\alias{ds.subsetdata}
\title{Generates a valid subsets of a dataset}
\usage{
ds.subsetdata(datasources = NULL, dataset = NULL, columns = NULL,
  newobj = NULL)
}
\arguments{
  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources.}

  \item{dataset}{a a string character, the name of the
  dataset.}

  \item{columns}{a character list vector that gives the
  names of the variables for which subsets are sougth.}

  \item{newobj}{the name of the output object. If this
  argument is set to NULL, the name of the new object is
  'subsets'.}
}
\value{
a message is displayed when the action is completed.
}
\description{

}
\details{
The function takes a dataframe and generates a subset
dataframes for all the factor variables. By default a
subset of the dataframe is created for each category of
each factor. It is possible to indicate the variable for
which a subset is sought by indicating their columns
indices. If the variables at the given indices are not
factors no subsets are generated.
}
\examples{
{

# load the login data
data(logindata)

# login and assign some variables to R
myvar <- list("DIS_DIAB","PM_BMI_CONTINUOUS","LAB_HDL", "GENDER")
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# Example 1: get all subsets from the table assigned above (by default the table name is 'D')
ds.subsetdata(datasources=opals, dataset="D")

#' # Example 2: get specific subsets from the table assigned above (by default the table name is 'D')
ds.subsetdata(datasources=opals, dataset="D", columns=list("GENDER"))
}
}
\author{
Gaye, A.
}

