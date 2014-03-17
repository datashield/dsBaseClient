\name{ds.propMean}
\alias{ds.propMean}
\title{Computes the mean proportion across categories}
\usage{
ds.propMean(datasources = NULL, dataset = NULL, outvar = NULL,
  covar1 = NULL, covar2 = NULL, covar3 = NULL, CI = 0.95)
}
\arguments{
  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources.}

  \item{dataset}{the name given to the dataset when it was
  assigned from opal to R.}

  \item{outvar}{a numeric or factor vector, the first
  covariate.}

  \item{covar1}{a numeric or factor vector, the second
  covariate.}

  \item{covar2}{a numeric or factor vector, the third
  covariate (optional).}

  \item{covar3}{a numeric or factor vector, the third
  covariate (optional).}

  \item{CI}{a value between 0 and 1 that set the threshold
  for statisitical significance.}
}
\value{
a list that contains the computed mean, standard error,
upper and lower confidence interval, degrees of freedom of
the fitted model and standard deviation.
}
\description{
This function calculates the mean proportion of a
continuous or binary for the differerent categories of some
categorical variables. The calculation can be carried out
for one continuous or binary outcone variable and up to 3
catagorical explanatory variables. can be processed.
}
\details{


The functions splits the input dataset into the single
categories and fits a glm model with a constant covariate.
This way, the regression coefficients from the glm fit are
the log odds-ratios (LO) of the outcome variable. The LO
and the standard error (se) are used to compute the lower
and upper limits of the confidence interval.
}
\examples{
{

# load that contains the login details
data(logindata)

# login and assign the whole dataset on the opal server
opals <- datashield.login(logins=logindata,assign=TRUE)

# Example 1: calculate the mean proportion for LAB_HDL across gender categories
ds.propMean(datasources=opals, dataset=quote(D), outvar=quote(D$LAB_HDL), covar1=quote(D$GENDER))

# Example 2: calculate the mean proportion for LAB_HDL across gender and bmi categories
ds.propMean(datasources=opals, dataset=quote(D), outvar=quote(D$LAB_HDL), covar1=quote(D$GENDER), covar2=quote(D$PM_BMI_CATEGORICAL))

# Example 3: calculate the mean proportion for LAB_HDL across gender bmi and diabetes status categories
ds.propMean(datasources=opals, dataset=quote(D), outvar=quote(D$LAB_HDL), covar1=quote(D$GENDER), covar2=quote(D$PM_BMI_CATEGORICAL), covar3=quote(D$DIS_DIAB))

# Example 3-5: repeat the above 3 examples but for a binary outcome variable: DIS_CVA (cardio-vascular acccident)
ds.propMean(datasources=opals, dataset=quote(D), outvar=quote(D$DIS_CVA), covar1=quote(D$GENDER))
\dontrun{ds.propMean(datasources=opals, dataset=quote(D), outvar=quote(D$DIS_CVA), covar1=quote(D$GENDER), covar2=quote(D$PM_BMI_CATEGORICAL))}
\dontrun{ds.propMean(datasources=opals, dataset=quote(D), outvar=quote(D$DIS_CVA), covar1=quote(D$GENDER), covar2=quote(D$PM_BMI_CATEGORICAL), covar3=quote(D$DIS_DIAB))}

}
}
\author{
Gaye, A.; Isaeva, J.
}

