\name{ds.meanByClass}
\alias{ds.meanByClass}
\title{Computes the mean and standard deviation across categories}
\usage{
ds.meanByClass(datasources = NULL, dataset = NULL, outvar = NULL,
  covar = NULL)
}
\arguments{
  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources.}

  \item{dataset}{the name given to the dataset when it was
  assigned from opal to R.}

  \item{outvar}{a character vector, the names of the
  continuous variables}

  \item{covar}{a character vector, the names of up to 3
  categorical variables}
}
\value{
a table that holds length of the continuous variables and
their mean and standard error for each class
}
\description{
This function calculates the mean and SD of a continuous
variable for each class of up to 3 categorical variables.
}
\details{


The functions splits the input dataset into the single
categories and calculates the mean and SD.
}
\examples{
{

# load that contains the login details
data(logindata)

# login and assign the whole dataset on the opal server
opals <- datashield.login(logins=logindata,assign=TRUE)

# Example 1: calculate the mean proportion for LAB_HDL across gender categories
ds.meanByClass(datasources=opals, dataset='D', outvar='LAB_HDL', covar='GENDER')

# Example 2: calculate the mean proportion for LAB_HDL across gender and bmi categories
ds.meanByClass(datasources=opals, dataset='D', outvar=c('LAB_HDL','LAB_TSC'), covar=c('GENDER'))

# Example 3: calculate the mean proportion for LAB_HDL across gender bmi and diabetes status categories
ds.meanByClass(datasources=opals, dataset='D', outvar=c('LAB_HDL','LAB_TSC'), covar=c('GENDER','PM_BMI_CATEGORICAL','DIS_DIAB'))

}
}
\author{
Gaye, A.
}

