\name{ds.subset}
\alias{ds.subset}
\title{Generates a valid subset of a table or a vector}
\usage{
ds.subset(datasources = NULL, subset = "subset", data = NULL,
  rows = NULL, cols = NUL, logical = NULL, threshold = NULL)
}
\arguments{
  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources.}

  \item{subset}{the name of the output object, a list that
  holds the subset object. If set to NULL the default name
  of this list is 'subsetObject'}

  \item{data}{a string character, the name of the dataframe
  or the factor vector and the range of the subset.}

  \item{rows}{a vector of two integers, the lower and upper
  index of the observations to extract. I the input data is
  a dataframe these are the rows and if it is a vector
  these are the entries.}

  \item{cols}{a numeric or character vector, if numeric the
  values are the indices of the columns to extract; the
  columns can also be refer to by their names by providing
  a character vector. This paramter is ignored if the input
  data is not a table.}

  \item{logical}{a charcater, the logical parameter to use
  if the user wishes to subset a vector using a logical
  operator. This parameter is ignored if the input data is
  not a vector.}

  \item{threshold}{a numeric, the threshold to use in
  conjunction with the logical parameter. This parameter is
  ignored if the input data is not a vector.}
}
\value{
a no data are return to the user but messages are printed
out.
}
\description{
The function uses the R classical subsetting with squared
brackets '[]' and allows also to subset using a logical
oprator and a threshold. The object to subset from must be
a vector (factor, numeric or charcater) or a table
(data.frame or matrix).
}
\details{


The user specifies the rows and columns to include in the
subset if the input object is a table and the indices of
the entries to include in the subset if the input object is
a vector. the name of a vector (i.e. a variable) can also
be provided with a logical operator and a threshold. In all
cases if the subset is not valid (i.e. contains less than
the allowed number of observations), the subset is not
generated.
}
\examples{
{

# load the login data
library(opal)
data(logindata)

# login and assign some variables to R
myvar <- list("DIS_DIAB","PM_BMI_CONTINUOUS","LAB_HDL", "GENDER")
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# Example 1: generate a subset of the assigned table (by default the table is named 'D')
# with the first 50 observations and the two first columns
ds.subset(datasources=opals, subset="subD", data="D", criteria="[1:50,1:2]")

# Example 2: generate a subset of the assigned table (by default the table is named 'D')
with the bmi values greater than or equal to 25.
ds.subset(datasources=opals, subset="subD", data="D", criteria="[PM_BMI_CONTINUOUS>=25]")

# Example 3: get the logarithmic values of the variable 'lab_hdl' and generate a subset with
the first 50 observations of that new vector.
ds.assign(opals, "logHDL", "log(D$LAB_HDL)")
ds.subset(datasources=opals, subset="subLAB_HDL", data="logHDL", criteria="[1:50]")

# Example 4: get the variable 'PM_BMI_CONTINUOUS' from the dataframe 'D' and generate a subset bmi
vector with bmi values greater than or equal to 25
ds.assign(opals, "BMI", "D$PM_BMI_CONTINUOUS")
ds.subset(datasources=opals, subset="subBMI", data="BMI", criteria=">=25")

ds.subclass(datasources=opals, subsets="subvectors", data=NULL, variables=NULL)
}
}
\author{
Gaye, A.
}

