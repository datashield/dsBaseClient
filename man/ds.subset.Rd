% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ds.subset.R
\name{ds.subset}
\alias{ds.subset}
\title{Generates a valid subset of a table or a vector}
\usage{
ds.subset(
  x = NULL,
  subset = "subsetObject",
  completeCases = FALSE,
  rows = NULL,
  cols = NULL,
  logicalOperator = NULL,
  threshold = NULL,
  datasources = NULL
)
}
\arguments{
\item{x}{a character, the name of the dataframe or the factor vector and the range of the subset.}

\item{subset}{the name of the output object, a list that holds the subset object. If set to NULL
the default name of this list is 'subsetObject'}

\item{completeCases}{a character that tells if only complete cases should be included or not.}

\item{rows}{a vector of integers, the indices of the rows to extract.}

\item{cols}{a vector of integers or a vector of characters; the indices of the columns to extract or their names.}

\item{logicalOperator}{a boolean, the logical parameter to use if the user wishes to subset a vector using a logical
operator. This parameter is ignored if the input data is not a vector.}

\item{threshold}{a numeric, the threshold to use in conjunction with the logical parameter. This parameter is ignored
if the input data is not a vector.}

\item{datasources}{a list of \code{\link[DSI]{DSConnection-class}} objects obtained after login. If the <datasources>
the default set of connections will be used: see \link[DSI]{datashield.connections_default}.}
}
\value{
no data are return to the user, the generated subset dataframe is stored on the server side.
}
\description{
The function uses the R classical subsetting with squared brackets '[]' and allows also to
subset using a logical oprator and a threshold. The object to subset from must be a vector (factor, numeric
or charcater) or a table (data.frame or matrix).
}
\details{
(1) If the input data is a table the user specifies the rows and/or columns to include in the subset; the columns can be
refered to by their names. Table subsetting can also be done using the name of a variable and a threshold (see example 3).
(2) If the input data is a vector and the parameters 'rows', 'logical' and 'threshold' are all provided the last two are ignored
(i.e. 'rows' has precedence over the other two parameters then).
IMPORTANT NOTE: If the requested subset is not valid (i.e. contains less than the allowed number of observations) all the values are
turned into missing values (NA). Hence an invalid subset is indicated by the fact that all values within it are set to NA.
}
\examples{
\dontrun{

  # load the login data
  data(logindata)

  # login and assign some variables to R
  myvar <- list("DIS_DIAB","PM_BMI_CONTINUOUS","LAB_HDL", "GENDER")
  conns <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

  # Example 1: generate a subset of the assigned dataframe (by default the table is named 'D')
  # with complete cases only
  ds.subset(x='D', subset='subD1', completeCases=TRUE)
  # display the dimensions of the initial table ('D') and those of the subset table ('subD1')
  ds.dim('D')
  ds.dim('subD1')

  # Example 2: generate a subset of the assigned table (by default the table is named 'D')
  # with only the variables
  # DIS_DIAB' and'PM_BMI_CONTINUOUS' specified by their name.
  ds.subset(x='D', subset='subD2', cols=c('DIS_DIAB','PM_BMI_CONTINUOUS'))

  # Example 3: generate a subset of the table D with bmi values greater than or equal to 25.
  ds.subset(x='D', subset='subD3', logicalOperator='PM_BMI_CONTINUOUS>=', threshold=25)

  # Example 4: get the variable 'PM_BMI_CONTINUOUS' from the dataframe 'D' and generate a
  # subset bmi
  # vector with bmi values greater than or equal to 25
  ds.assign(toAssign='D$PM_BMI_CONTINUOUS', newobj='BMI')
  ds.subset(x='BMI', subset='BMI25plus', logicalOperator='>=', threshold=25)

  # Example 5: subsetting by rows:
  # get the logarithmic values of the variable 'lab_hdl' and generate a subset with
  # the first 50 observations of that new vector. If the specified number of row is
  # greater than the total
  # number of rows in any of the studies the process will stop.
  ds.assign(toAssign='log(D$LAB_HDL)', newobj='logHDL')
  ds.subset(x='logHDL', subset='subLAB_HDL', rows=c(1:50))
  # now get a subset of the table 'D' with just the 100 first observations
  ds.subset(x='D', subset='subD5', rows=c(1:100))

  # clear the Datashield R sessions and logout
  datashield.logout(conns)

}

}
\seealso{
\link{ds.subsetByClass} to subset by the classes of factor vector(s).

\link{ds.meanByClass} to compute mean and standard deviation across categories of a factor vectors.
}
\author{
Gaye, A.
}
