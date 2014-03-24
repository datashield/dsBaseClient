\name{ds.subclass}
\alias{ds.subclass}
\title{Generates valid subset(s) of a dataframe or a factor}
\usage{
ds.subclass(datasources = NULL, subsets = "subsClasses", data = NULL,
  variables = NULL)
}
\arguments{
  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources.}

  \item{subsets}{the name of the output object, a list that
  holds the subset objects. If set to NULL the default name
  of this list is 'subsClasses'}

  \item{data}{a string character, the name of the data
  frame or the vector to generate subsets from.}

  \item{variables}{a vector of string characters, the
  name(s) of the variables to subset by.}
}
\value{
a no data are return to the user but messages are printed
out.
}
\description{
The function takes a categorical variable or a data frame
as input and generates subset(s) variables or data frames
for each category.
}
\details{


If the input data object is a data frame it is possible to
specify the variables to subset on. If a subset is not
'valid' all its the values are reported as missing (i.e.
NA), the name of the subsets is labelled as '_INVALID'.
Subsets are considered invalid if the number of
observations it holds are less than the agreed threshold
(e.g. 5).
}
\examples{
{

# load the login data
library(opal)
data(logindata)

# login and assign some variables to R
myvar <- list("DIS_DIAB","PM_BMI_CONTINUOUS","LAB_HDL", "GENDER")
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# Example 1: generate all possible subsets from the table assigne above
ds.subclass(datasources=opals, subsets="subclasses", data="D")

# Example 2: subset the table initially assigned by the variable 'GENDER'
ds.subclass(datasources=opals, subsets="subtables", data="D", variables="GENDER")

# Example 3: generate a new variable 'gender' and split it into two vectors: males and females
datashield.assign(opals, "gender", quote(D$GENDER))
ds.subclass(datasources=opals, subsets="mf.tables", data="gender")

ds.subclass(datasources=opals, subsets="subvectors", data=NULL, variables=NULL)
}
}
\author{
Gaye, A.
}

