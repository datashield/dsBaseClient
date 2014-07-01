\name{ds.histogram}
\alias{ds.histogram}
\title{Plots a histogram}
\usage{
ds.histogram(x = NULL, type = "combine", datasources = NULL)
}
\arguments{
  \item{x}{a charcater, the name of the vector of values
  for which the histogram is desired.}

  \item{type}{a character which represent the type of graph
  to display. If \code{type} is set to 'combine', a
  histogram that merges the single plot is displayed. Each
  histogram is plotted separately if If \code{type} is set
  to 'split'.}

  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources.}
}
\value{
one or more histogram objects and plots depending on the
argument \code{type}
}
\description{
This function plots histogram without outliers
}
\details{


It calls a datashield server side function that produces
the histogram objects to plot. The objects to plot do not
contain bins with counts smaller than the allowed
thresholdd. The function allows for the user to plot
disctinct histograms (one for each study) or a combine
histogram that merges the single plots.
}
\examples{
{

# load that contains the login details
data(logindata)

# login and assign specific variable(s)
myvar <- list('LAB_TSC', 'LAB_HDL')
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# Example 1: plot a combined histogram of the variable 'LAB_TSC'  default behaviour
ds.histogram(x='D$LAB_TSC')

# Example 2: Plot the histograms of LAB_TSC separately (one per study)
ds.histogram(x='D$LAB_TSC', type='split')

# Example 2: plot a combined histogram of the variable 'LAB_HDL'  default behaviour
ds.histogram(x='D$LAB_HDL')

# Example 3: plot the histograms of LAB_HDL separately (one per study)
ds.histogram(x='D$LAB_HDL', type='split')

# clear the Datashield R sessions and logout
datashield.logout(opals)

}
}
\author{
Gaye, A.
}

