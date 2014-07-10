\name{ds.subclass}
\alias{ds.subclass}
\title{Generates valid subset(s) of a data frame or a factor}
\usage{
ds.subclass(x = NULL, subsets = "subClasses", variables = NULL,
  datasources = NULL)
}
\arguments{
  \item{x}{a character, the name of the dataframe or the
  vector to generate subsets from.}

  \item{variables}{a vector of string characters, the
  name(s) of the variables to subset by.}

  \item{subsets}{the name of the output object, a list that
  holds the subset objects. If set to NULL the default name
  of this list is 'subClasses'.}

  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources.}
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
data(logindata)

# login and assign some variables to R
myvar <- list('DIS_DIAB','PM_BMI_CONTINUOUS','LAB_HDL', 'GENDER')
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# Example 1: generate all possible subsets from the table assigne above
ds.subclass(x='D', subsets='subclasses')

# Example 2: subset the table initially assigned by the variable 'GENDER'
ds.subclass(x='D', subsets='subtables', variables='GENDER')

# Example 3: generate a new variable 'gender' and split it into two vectors: males and females
ds.assign(newobj='gender', toAssign='D$GENDER')
ds.subclass(x='gender', subsets='subvectors')

# clear the Datashield R sessions and logout
datashield.logout(opals)

}
}
\author{
Gaye, A.
}

