\name{table1dhelper2}
\alias{table1dhelper2}
\title{Combines output of server side function \code{table1d.ds}}
\usage{
table1dhelper2(input)
}
\arguments{
  \item{input}{a list object returned by the server side
  function \code{table1d.ds}}
}
\value{
a matrix containing study specific and overall (combined)
tables of counts based ONLY on data from studies where all
table categories are VALID.
}
\description{
this is an INTERNAL function required by the client
function \code{ds.table1D}.
}
\details{


It gathers the objects returned by \code{table1dDS}
}
\author{
Burton, P.; Gaye, A.
}
\keyword{internal}

