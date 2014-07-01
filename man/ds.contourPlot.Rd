\name{ds.contourPlot}
\alias{ds.contourPlot}
\title{Generates a contour plot}
\usage{
ds.contourPlot(x = NULL, y = NULL, type = "combine", show = "all",
  numints = 20, datasources = NULL)
}
\arguments{
  \item{x}{a character, the name of a numerical vector.}

  \item{y}{a character, the name of a numerical vector.}

  \item{type}{a character which represents the type of
  graph to display. If \code{type} is set to 'combine', a
  combined contour plot displayed and if \code{type} is set
  to 'split', each conntour is plotted separately.}

  \item{show}{a character which represents where the plot
  should focus. If \code{show} is set to 'all', the ranges
  of the variables are used as plot limits. If \code{show}
  is set to 'zoomed', the plot is zoomed to the region
  where the actual data are.}

  \item{numints}{a number of intervals for a density grid
  object.}

  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources.}
}
\value{
a contour plot
}
\description{
Generates a countour plot of the pooled data or one plot
for each dataset.
}
\details{


The function first generates a density grid and uses it to
plot the graph. Cells of the grid density matrix that hold
a count of less than the filter set by DataSHIELD (usually
5) are considered invalid and turned into 0 to avoid
potential disclosure. A message is printed to inform the
user about the number of invalid cells. The ranges returned
by each study and used in the process of getting the grid
density matrix are not the exact minumum and maximum values
but rather close approximates of the real minimum and
maximum value. This was done to reduce the risk of
potential disclosure.
}
\examples{
{

# load the file that contains the login details
data(logindata)

# login and assign specific variables(s)
# (by default the assigned dataset is a dataframe named 'D')
myvar <- list("LAB_TSC","LAB_HDL")
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# Example 1: generate a contour plot of the pooled data (default)
ds.contourPlot(x='D$LAB_TSC', y='D$LAB_HDL')
# now produce the same plot but zoom in
ds.contourPlot(x='D$LAB_TSC', y='D$LAB_HDL', show='zoomed')

# Example 2: generate a contour plot where each study is plotted seaparately
ds.contourPlot(x='D$LAB_TSC', y='D$LAB_HDL', type='split')
# now produce the same plots but zoom in
ds.contourPlot(x='D$LAB_TSC', y='D$LAB_HDL', type='split', show='zoomed')

# Example 3: generate a contour plot with a less dense grid (default numints is 20)
ds.contourPlot(x='D$LAB_TSC', y='D$LAB_HDL', type='split', numints=15)

# clear the Datashield R sessions and logout
datashield.logout(opals)

}
}
\author{
Isaeva, J.; Gaye, A.; Burton, P.
}

