\name{ds.cor}
\alias{ds.cor}
\title{Computes the correlation between two or more vectors}
\usage{
ds.cor(x = NULL, y = NULL, naAction = "pairwise.complete.obs",
  datasources = NULL)
}
\arguments{
  \item{x}{a character, the name of a numerical vector,
  matrix or dataframe}

  \item{y}{NULL (default) or the name of a vector, matrix
  or data frame with compatible dimensions to x.}

  \item{naAction}{a character string giving a method for
  computing covariances in the presence of missing values.
  This must be one of the strings: "everything", "all.obs",
  "complete.obs", "na.or.complete", or
  "pairwise.complete.obs". The default value is set to
  "pairwise.complete.obs"}

  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources.}
}
\value{
a list containing the results of the test
}
\description{
This is similar to the R base function 'cor'.
}
\details{


In addition to computing correlations this function, unlike
the R base function 'cor', produces a table outlining the
number of complete cases to allow for the user to make a
decision about the 'relevance' of the correlation based on
the number of complete cases included in the correlation
calculations.
}
\examples{
{

# load that contains the login details
data(logindata)

# login and assign specific variable(s)
# (by default the assigned dataset is a dataframe named 'D')
myvar <- list('LAB_HDL', 'LAB_TSC', 'GENDER')
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# Example 1: generate the correlation matrix for the assigned dataset 'D'
# which contains 4 vectors (2 continuous and 1 categorical)
ds.cor(x='D')

# Example 2: calculate the correlation between two vectors (first assign some vectors from the dataframe 'D')
ds.assign(newobj='labhdl', toAssign='D$LAB_HDL')
ds.assign(newobj='labtsc', toAssign='D$LAB_TSC')
ds.assign(newobj='gender', toAssign='D$GENDER')
ds.cor(x='labhdl', y='labtsc')
ds.cor(x='labhdl', y='gender')

# clear the Datashield R sessions and logout
datashield.logout(opals)

}
}
\author{
Gaye, A.
}

