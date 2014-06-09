\name{ds.densityGrid}
\alias{ds.densityGrid}
\title{Generates a density grid with or without a priori defined limits}
\usage{
ds.densityGrid(xvect = NULL, yvect = NULL, numints = 20,
  type = "combine", datasources = NULL)
}
\arguments{
  \item{xvect}{a character the name of numerical vector}

  \item{yvect}{a character the name of numerical vector}

  \item{numints}{an integer, the number of intervals for
  the grid density object, by default is 20.}

  \item{type}{a character which represent the type of graph
  to display. If \code{type} is set to 'combine', a pooled
  grid density matrix is generated and one grid density
  matrix is generated for each study if \code{type} is set
  to 'split'.}

  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources.}
}
\value{
a grid density matrix is returned
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

# Example1: generate a combined grid density object (the default behaviour)
ds.densitygrid(xvect='D$LAB_TSC', yvect='D$LAB_HDL')

# Example2: generate a grid density object for each study separately
ds.densitygrid(xvect='D$LAB_TSC', yvect='D$LAB_HDL', type="split")

# Example3: generate a grid density object where the number of intervals is set to 15, for each study separately
ds.densitygrid(xvect='D$LAB_TSC', yvect='D$LAB_HDL', type="split", numints=15)
}
}
\author{
Isaeva, J.; Gaye, A.
}

