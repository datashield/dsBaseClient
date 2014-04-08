\name{ds.append2df}
\alias{ds.append2df}
\title{Appends a vector/column to a dataframe}
\usage{
ds.append2df(datasources = NULL, dataset = NULL, xvect = NULL,
  replace = FALSE)
}
\arguments{
  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources.}

  \item{dataset}{the input dataset, the table to append a
  column to.}

  \item{xvect}{a numeric or factor vector}

  \item{replace}{a character which tells if the intitial
  dataframe should be replaced by the the newly dataframe
  (i.e. the one with the additional column). If
  \code{replace} is set to 'FALSE', default, the initial
  dataframe is not replaced and the new dataframe is named
  as exaplained in the 'details' section of the
  documentation. if \code{replace} is set to 'TRUE', the
  initial dataframe is replaced (i.e. the new dataframe
  takes the name of the initial one)}
}
\value{
a message is displayed when the action is completed.
}
\description{

}
\details{
The function appends a numeric or factor vector to a
dataframe. The initial dataframe is replaced by the new
dataframe if the argument 'replace' is set to 'TRUE';
otherwise a new dataframe is generated and named after the
intitial dataframe with the addition of a suffixe 'new';
for example if the initial dataframe is 'D' the new
dataframe will be 'Dnew'.
}
\examples{
{

# load that contains the login details
data(logindata)

# login and assign specific variable(s)
myvar <- list("LAB_HDL","LAB_TSC")
opals <- datashield.login(logins=logindata, assign=TRUE, variables=myvar, symbol="D")

# generate a new variable (e.g. a mean centered LAB_HDL)
# get the mean of LAB_HDL
mean.lab_hdl <- ds.mean(opals, 'D$LAB_HDL', type='split')
# center LAB_HDL for each study
for(i in 1:length(opals)){
  call.object <- call("-", quote(D$LAB_HDL), mean.lab_hdl[[i]])
  datashield.assign(opals[i], "lab_hdl.c", call.object)
}

# Example 1: append the variable 'lab_hdl.c' to 'D' and generate 'Dnew'
ds.append2df(datasources=opals, quote(D), quote(lab_hdl.c))

# Example 2: append the variable 'lab_hdl.c' to 'D' and update 'D'
ds.append2df(datasources=opals, dataset=quote(D), xvect=quote(lab_hdl.c), replace=quote(TRUE))
}
}
\author{
Gaye, A.
}

