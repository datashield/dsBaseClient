\name{ds.histogram}
\alias{ds.histogram}
\title{Plots a histogram}
\usage{
  ds.histogram(datasources = NULL, xvect = NULL,
    type = "combine")
}
\arguments{
  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources.}

  \item{xvect}{vector of values for which the histogram is
  desired.}

  \item{type}{a character which represent the type of graph
  to display. If \code{type} is set to 'combine', a
  histogram that merges the single plot is displayed. Each
  histogram is plotted separately if If \code{type} is set
  to 'split'.}
}
\value{
  one or more histogram plot depending on the argument
  \code{type}
}
\description{
  This function plots histogram of the given data values.
  It calls a datashield server side function that produces
  the histogram objects to plot. The objects to plot do not
  contain bins with counts < 5. The function allows for the
  user to plot disctinct histograms (one for each study) or
  a combine histogram that merges the single plots.
}
\examples{
{

# load that contains the login details
data(logindata)

# login and assign specific variable(s)
myvar <- list("LAB_TSC", "LAB_HDL")
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# Example 1: plot a combined histogram of the variable 'LAB_TSC' - default behaviour
ds.histogram(datasources=opals, xvect=quote(D$LAB_TSC))

# Example 2: Plot the histograms of LAB_TSC separately (one per study)
ds.histogram(datasources=opals, xvect=quote(D$LAB_TSC), type="split")

# Example 2: plot a combined histogram of the variable 'LAB_HDL' - default behaviour
ds.histogram(datasources=opals, xvect=quote(D$LAB_HDL))

# Example 3: plot the histograms of LAB_HDL separately (one per study)
ds.histogram(datasources=opals, xvect=quote(D$LAB_HDL), type="split")

# Example 4: Plot the histograms of the first and second study
 ds.histogram(datasources=opals[1:2], xvect=quote(D$LAB_TSC), type="split")

# Example 5: Plot the histogram of the third study only
 ds.histogram(datasources=opals[3], xvect=quote(D$LAB_TSC), type="split")
}
}
\author{
  Gaye, A.
}

