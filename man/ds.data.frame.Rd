\name{ds.data.frame}
\alias{ds.data.frame}
\title{Creates data frames}
\usage{
  ds.data.frame(datasources = NULL, vector = NULL,
    newobj = NULL)
}
\arguments{
  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources.}

  \item{vector}{a vector that holds the objects to be
  combined (together with additional options)}

  \item{newobj}{the name of the output object. If this
  argument is set to \code{NULL}, the name of the new
  object is 'dframe'.}
}
\value{
  a message is displayed when the action is completed.
}
\description{
  Creates data frames
}
\examples{
{

# load the file that contains the login details
data(logindata)

# login and assign the required variables to R
myvar <- list("LAB_TSC","LAB_HDL")
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# combine the 'LAB_TSC' and 'LAB_HDL' variables into a data frame
myvect <- quote(data.frame(D$LAB_TSC, D$LAB_HDL))
ds.data.frame(datasources=opals, vector=myvect)

}
}
\author{
  Gaye, A. (amadou.gaye
}

