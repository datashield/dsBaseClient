\name{ds.heatmapplot}
\alias{ds.heatmapplot}
\title{Generates a heatmap plot for merged datasets}
\usage{
  ds.heatmapplot(opals, xvect, yvect, type = "combine",
    numints = 20)
}
\arguments{
  \item{opals}{character strings that represent the URL of
  the servers where the study datasets are stored.}

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
opals <- ds.login(logins=logindata,assign=TRUE,variables=myvar)

# Example1: generate a combined heatmapplot
ds.heatmapplot(opals, quote(D$LAB_TSC), quote(D$LAB_HDL), type="combine")

# Example2: generate a heatmapplot where each study is plotted seaparately
ds.heatmapplot(opals, quote(D$LAB_TSC), quote(D$LAB_HDL), type="split")

# Example3: generate a heatmapplot with a less dense drid
ds.heatmapplot(opals, quote(D$LAB_TSC), quote(D$LAB_HDL), type="split", numints=15)
}
}
\author{
  Isaeva, J. and Gaye, A.
}

