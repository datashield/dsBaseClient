\name{ds.heatmapplot}
\alias{ds.heatmapplot}
\title{Generates a heatmap plot for merged datasets}
\usage{
  ds.heatmapplot(datasources, xvect, yvect,
    type = "combine", numints = 20)
}
\arguments{
  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources.}

  \item{xvect}{a numerical vector}

  \item{yvect}{a numerical vector}

  \item{type}{a character which represents the type of
  graph to display. If \code{type} is set to 'combine', a
  combined heatmap plot displayed and if \code{type} is set
  to 'split', each heatmap is plotted separately.}

  \item{numints}{a number of intervals for a density grid
  object}
}
\value{
  a heatmap plot
}
\description{
  Generates a heatmap plot for merged datasets
}
\examples{
{
# load the file that contains the login details
data(logindata)

# login and assign the required variables to R
myvar <- list("LAB_TSC","LAB_HDL")
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# Example1: generate a combined heatmapplot
ds.heatmapplot(datasources=opals, quote(D$LAB_TSC), quote(D$LAB_HDL), type="combine")

# Example2: generate a heatmapplot where each study is plotted seaparately
ds.heatmapplot(datasources=opals, quote(D$LAB_TSC), quote(D$LAB_HDL), type="split")

# Example3: generate a heatmapplot with a less dense drid
ds.heatmapplot(datasources=opals, quote(D$LAB_TSC), quote(D$LAB_HDL), type="split", numints=15)
}
}
\author{
  Isaeva, J. and Gaye, A.
}

