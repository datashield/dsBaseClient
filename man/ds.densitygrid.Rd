\name{ds.densitygrid}
\alias{ds.densitygrid}
\title{Generates a density grid with or without a priori defined limits}
\usage{
ds.densitygrid(datasources = NULL, xvect = NULL, yvect = NULL,
  numints = 20, type = "combine")
}
\arguments{
  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources.}

  \item{xvect}{a numerical vector}

  \item{yvect}{a numerical vector}

  \item{numints}{a number of intervals for the grid density
  object, by default is 20}

  \item{type}{a character which represent the type of graph
  to display. If \code{type} is set to 'combine', a
  histogram that merges the single plot is displayed. Each
  histogram is plotted separately if If \code{type} is set
  to 'split'.}
}
\value{
a global grid density matrix across all studies or a one
grid density matrix for each study
}
\description{
This function generates a grid density object which can
then be used to produced a heatmap or contourplots. In
cells with a count > 0 and < 5 are considered invalid and
the count is set to 0. The function prints the number of
invalid cells in for participating study.
}
\details{


In DataSHIELD the user does not have access to the
micro-data so and extreme values such as the maximum and
the minimum are potentially disclosive so this function
does not allow for the user to set the limits of the
density grid and the minimum and maximum values of the x
and y vectors. These elements are set by the server side
function \code{densitygrid.ds} to 'valid' values (i.e.
values that do not lead to leakage of micro-data to the
user).
}
\examples{
{
# load the file that contains the login details
data(logindata)

# login and assign the required variables to R
myvar <- list("LAB_TSC","LAB_HDL")
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# Example1: generate a combined grid density object
ds.densitygrid(datasources=opals, quote(D$LAB_TSC), quote(D$LAB_HDL), type="combine")

# Example2: generate a grid density object for each study separately
ds.densitygrid(datasources=opals, quote(D$LAB_TSC), quote(D$LAB_HDL), type="split")

# Example3: generate a grid density object where the number of intervals is set to 15, for each study separately
ds.densitygrid(datasources=opals, quote(D$LAB_TSC), quote(D$LAB_HDL), type="split", numints=15)
}
}
\author{
Isaeva, J.; Gaye, A.
}

