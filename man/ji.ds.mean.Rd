\name{ji.ds.mean}
\alias{ji.ds.mean}
\title{Calculates a mean of a given vector (for several studies separately or combined)}
\usage{
  ji.ds.mean(datasources, xvect, type = "combine")
}
\arguments{
  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources.}

  \item{xvect}{a numerical vector}

  \item{type}{a character which represents the type of
  graph to display. If \code{type} is set to 'combine', a
  combined heatmap plot displayed and if \code{type} is set
  to 'split', each heatmap is plotted separately.}
}
\value{
  a mean value
}
\description{
  Calculates a mean of a given vector (for several studies
  separately or combined)
}
\author{
  Isaeva, J.
}

