\name{ds.subset}
\alias{ds.subset}
\title{Generates a valid subset of a table or a vector}
\usage{
ds.subset(datasources = NULL, subset = "subsetObject", data = NULL,
  complete = FALSE, rows = NULL, cols = NULL, logical = NULL,
  threshold = NULL)
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

  \item{complete}{a boolean that tells if only complete
  cases should be included or not.}

  \item{rows}{a vector of integers, the indices of the rows
  de extract.}

  \item{cols}{a vector of integers or characters; the
  indices of the columns to extract or the names of the
  columns (i.e. names of the variables to extract).}

  \item{logical}{a charcater, the logical parameter to use
  if the user wishes to subset a vector using a logical
  operator. This parameter is ignored if the input data is
  not a vector.}

  \item{threshold}{a numeric, the threshold to use in
  conjunction with the logical parameter. This parameter is
  ignored if the input data is not a vector.}
}
\value{
a no data are return to the user, the generated subset is
stored on the server side.
}
\description{
The function uses the R classical subsetting with squared
brackets '[]' and allows also to subset using a logical
oprator and a threshold. The object to subset from must be
a vector (factor, numeric or charcater) or a table
(data.frame or matrix).
}
\details{


If the input data is a table: The user specifies the rows
and/or columns to include in the subset if the input object
is a table; the columns can be refered to by their names.
The name of a vector (i.e. a variable) can also be provided
with a logical operator and a threshold (see example 3). If
the input data is a vector: when the parameters 'rows',
'logical' and 'threshold' are all provided the last two are
ignored ('rows' has precedence over the other two
parameters then). IMPORTANT NOTE: If the requested subset
is not valid (i.e. contains less than the allowed number of
observations) or if it does not have any observations, the
subset is not generated, rather a table or a vector of
missing values is generated to allow for any subsequent
process using the output of the function to proceed, after
informing the user via a message.
}
\examples{
{

# load the login data
data(logindata)

# login and assign some variables to R
myvar <- list("DIS_DIAB","PM_BMI_CONTINUOUS","LAB_HDL", "GENDER")
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# Example 1: generate a subset of the assigned dataframe (by default the table is named 'D') with complete cases only
#' ds.subset(datasources=opals, subset='subD', data='D', complete=TRUE)

# Example 2: generate a subset of the assigned table (by default the table is named 'D') with only two variables
# specified by their names
ds.subset(datasources=opals, subset='subD', data='D', cols <- c('DIS_DIAB','PM_BMI_CONTINUOUS'))

# Example 3: generate a subset of the table D with bmi values greater than or equal to 25.
ds.subset(datasources=opals, subset='subD', data='D', logical='PM_BMI_CONTINUOUS>=', threshold=25)

# Example 4: get the logarithmic values of the variable 'lab_hdl' and generate a subset with
the first 50 observations of that new vector. If the specified number of row is greater than the total
number of rows in any of the studies the process will stop.
ds.assign(opals, 'logHDL', 'log(D$LAB_HDL)')
ds.subset(datasources=opals, subset="subLAB_HDL", data="logHDL", rows=c(1:50))

# Example 5: get the variable 'PM_BMI_CONTINUOUS' from the dataframe 'D' and generate a subset bmi
vector with bmi values greater than or equal to 25
ds.assign(opals, "BMI", "D$PM_BMI_CONTINUOUS")
ds.subset(datasources=opals, subset='BMI25plus', data='BMI', logical='>=', threshold=25)

}
}
\author{
Gaye, A.
}

