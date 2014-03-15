\name{ds.quantilemean}
\alias{ds.quantilemean}
\title{Plots a histogram}
\usage{
ds.quantilemean(datasources = NULL, xvect = NULL, type = "combine")
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
This function plots histogram of the given data values. It
calls a datashield server side function that produces the
histogram objects to plot. The objects to plot do not
contain bins with counts < 5. The function allows for the
user to plot disctinct histograms (one for each study) or a
combine histogram that merges the single plots.
}
\details{

}
\examples{
{

# load that contains the login details
data(logindata)

# login and assign specific variable(s)
myvar <- list("LAB_HDL")
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# Example 1: plot a combined histogram of the variable 'LAB_HDL' - default behaviour
ds.quantilemean(datasources=opals, xvect=quote(D$LAB_HDL))

# Example 2: Plot the histograms separately (one per study)
ds.quantilemean(datasources=opals, xvect=quote(D$LAB_HDL), type="split")

}
}
\author{
Gaye, A.
}

