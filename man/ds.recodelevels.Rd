\name{ds.recodelevels}
\alias{ds.recodelevels}
\title{recodes a categorical variable}
\usage{
  ds.recodelevels(datasources = NULL, xvect = NULL,
    newlabels = NULL, newobj = NULL)
}
\arguments{
  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources.}

  \item{xvect}{a factor}

  \item{newlabels}{a string vector with new labels for the
  levels If \code{newlabels} is not specified, the naming
  of original levels is amended - numbering is added
  (0_..., 1_..., 2_... etc.)}

  \item{newobj}{the name of the new variable. If this
  argument is set to NULL, the name of the new variable is
  the name of the input variable with the suffixe
  '_recoded' (e.g. 'PM_BMI_CATEGORICAL_recoded', if input
  variable's name is 'PM_BMI_CATEGORICAL')}
}
\value{
  a factor vector with new labels for levels
}
\description{
  this function recodes levels of a categorical variable
  with new given labels.
}
\examples{
{

# load that contains the login details
data(logindata)

# login and assign specific variable(s)
myvar <- list("PM_BMI_CATEGORICAL")
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# rename the levels of PM_BMI_CATEGORICAL
ds.recodelevels(opals, quote(D$PM_BMI_CATEGORICAL), newlabels=c('normal', 'overweight', 'obesity'), 'bmi_new')

}
}
\author{
  Isaeva, I.; Gaye, A.
}

