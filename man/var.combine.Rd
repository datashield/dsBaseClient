\name{var.combine}
\alias{var.combine}
\title{Computes the variance of a variable}
\usage{
  var.combine(opals, x)
}
\arguments{
  \item{opals}{character strings that represent the URL of
  the servers where the study datasets are stored}

  \item{x}{a numeric vector, matrix or data frame}
}
\value{
  a
}
\description{
  Calculates the variance of a variable across several
  non-pooled datasets
}
\details{
  If xvect contains missing values, the latter are omitted
  (na.rm=TRUE). If xvect is not present in any of the data
  sets, the variance of xvect is calculated only for those
  data sets that contain xvect. In addition, a warning
  message is produced.
}
\author{
  Isaeva, J.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  The New S Language. Wadsworth & Brooks/Cole.
}

