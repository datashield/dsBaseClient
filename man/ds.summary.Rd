\name{ds.summary}
\alias{ds.summary}
\title{Generates the summary of a numeric or factor vector}
\usage{
ds.summary(datasources = NULL, xvect = NULL, type = "combine")
}
\arguments{
  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources.}

  \item{xvect}{a numeric or factor variable}

  \item{type}{a character which represents the type of
  graph to display. If \code{type} is set to 'combine', a
  summary of the pooled data is displayed and if
  \code{type} is set to 'split', the summary is generate
  for each study separately.}
}
\value{
a non disclosive summary
}
\description{

}
\details{
This function call \code{ds.quantilemean} if the input
vector is a numeric (i.e. continuous variable) and
\code{ds.table1d} if it is a factor (i.e. categorical
variable). The user can choose the generate a pooled or
split summary (one for each study).
}
\examples{
{
# load the login data
data(logindata)

# login and assign specific variable(s)
myvar <- list("LAB_TSC", "GENDER")
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# Example 1: Produce the global summary of a continuous variable
ds.summary(datasources=opals, xvect=quote(D$LAB_TSC))

# Example 2: Produce the global summary of a factor variable
ds.summary(datasources=opals, xvect=quote(D$GENDER))
}
}
\author{
Gaye, A.
}

