\name{ds.makeBinary}
\alias{ds.makeBinary}
\title{Generates a valid binary variable from a continuous variable}
\usage{
ds.makeBinary(datasources = NULL, xvect = NULL, threshold = NULL,
  operator = NULL, newobj = NULL)
}
\arguments{
  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources.}

  \item{xvect}{a numeric vector.}

  \item{operator}{an string character: '>', '<', '>=',
  '<=', '==' or '!='.}

  \item{threshold}{a numeric value that sets the threshold
  level.}

  \item{newobj}{the name of the output object. If this
  argument is set to NULL, the name of the new object is
  the name of the input variable with a suffixe '_subset'.}
}
\value{
a message is displayed when the action is completed.
}
\description{
The functions takes as inpout a continuous variable and
based on a criterion assigns the value 1 to observation
that met the criterion and the value 0 otherwise. Hence
generating a binary variable.
}
\details{


This function runs only if the set threshold criterion is
met or not met by more than one observation. The
observation that met the criterion as assigned the value
'1' and other onservations are assigned '0'
}
\examples{
{

# load the login data
data(logindata)

# login and assign some variables to R
myvar <- list("PM_BMI_CONTINUOUS", "LAB_HDL")
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# generate a binary variable variable using the threshold bmi >= 25
ds.makeBinary(datasources=opals, xvect=quote(D$PM_BMI_CONTINUOUS), operator=">=", threshold=25)

}
}
\author{
Gaye, A.
}

