\name{ds.amendLevels}
\alias{ds.amendLevels}
\title{Ensures factors have same classes across studies}
\usage{
ds.amendLevels(datasources = NULL, xvect = NULL, newvarname = NULL)
}
\arguments{
  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources.}

  \item{xvect}{a factor variable.}

  \item{newvarname}{name of the variable to assigned the
  created factor to.}
}
\value{
a list of \code{factor} vectors or one factor vector.
}
\description{

}
\details{
This function checks if a factor variable has the same
length accross the collaborating studies; if not it adds
the missing level(s) to make up for the missing class(es).
This is necessary for certain function like \code{ds.glm}
which require the same classes across the studies to
combine.
}
\examples{
{

# create a login table for the test data that has the features we want to test
server <- c("study1", "study2", "study3")
url <- c("http://54.242.140.255","http://54.242.46.59", "http://23.22.215.42")
user <- c("administrator", "administrator", "administrator")
password <- c("password", "password", "password")
table <- c("Test.CNSIM", "Test.CNSIM", "Test.CNSIM")
logindata <- data.frame(server,url,user,password,table)
# login and assign specific variable(s)
myvar <- list("PM_BMI_CATEGORICAL")
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
# check the number of levels for the variable PM_BMI_CATEGORICAL in each study
# levels of PM_BMI_CATEGORICAL in the 3 studies
datashield.aggregate(opals[1], quote(levels(D$PM_BMI_CATEGORICAL)))
datashield.aggregate(opals[2], quote(levels(D$PM_BMI_CATEGORICAL)))
datashield.aggregate(opals[3], quote(levels(D$PM_BMI_CATEGORICAL)))

#create a factor vector of the variable 'PM_BMI_CATEGORICAL', one for each study
ds.amendLevels(opals, xvect=quote(D$PM_BMI_CATEGORICAL))

# the name of the newly created variable is the previous one with the suffixe '_f'
datashield.aggregate(opals[1], quote(levels(PM_BMI_CATEGORICAL_f)))
datashield.aggregate(opals[2], quote(levels(PM_BMI_CATEGORICAL_f)))
datashield.aggregate(opals[3], quote(levels(PM_BMI_CATEGORICAL_f)))
}
}
\author{
Gaye, A.
}

