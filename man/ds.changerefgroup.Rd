\name{ds.changerefgroup}
\alias{ds.changerefgroup}
\title{changes a reference level of a factor}
\usage{
ds.changerefgroup(datasources = NULL, xvect = NULL, ref = NULL,
  newobj = NULL)
}
\arguments{
  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources.}

  \item{xvect}{a factor}

  \item{ref}{the reference level}

  \item{newobj}{the name of the new variable. If this
  argument is set to NULL, the name of the new variable is
  the name of the input variable with the suffixe '_newref'
  (e.g. 'PM_BMI_CATEGORICAL_newref', if input variable's
  name is 'PM_BMI_CATEGORICAL')}
}
\value{
a factor of the same length as xvect
}
\description{
this function is similar to R function \code{relevel}, but
in addition addes numbering to the levels so that they are
displayed in the right order when creating cross-tables.
}
\details{

}
\examples{
{

# load that contains the login details
data(logindata)

# login and assign specific variable(s)
myvar <- list("PM_BMI_CATEGORICAL")
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# rename the levels of PM_BMI_CATEGORICAL and make "obesity" as a reference level
ds.recodelevels(opals, quote(D$PM_BMI_CATEGORICAL), newlabels=c('normal', 'overweight', 'obesity'), 'bmi_new')
ds.changerefgroup(opals, quote(bmi_new), ref='2_obesity', newobj = 'bmi_ob')
ds.levels(opals, quote(bmi_ob))

# or without renaming the levels (group "3" as a reference level)
ds.changerefgroup(opals, quote(D$PM_BMI_CATEGORICAL), ref='3')
ds.levels(opals, quote(PM_BMI_CATEGORICAL_newref))

}
}
\author{
Isaeva, I.; Gaye, A.
}

