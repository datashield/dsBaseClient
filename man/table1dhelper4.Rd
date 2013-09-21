\name{table1dhelper4}
\alias{table1dhelper4}
\title{Finalizes the output of the function \code{table1d.ds}}
\usage{
  table1dhelper4(input)
}
\arguments{
  \item{input}{a list object returned by the server side
  function \code{table1d.ds}}
}
\value{
  a list that contains the elements returned by the client
  function \code{ds.table1d}
}
\description{
  this is an INTERNAL function required by the client
  function \code{ds.table1d}.
}
\details{
  the function is not 'exported' and hence not visible to
  user and is only called by \code{ds.table1d}
}
\author{
  Burton, P.; Gaye, A.
}

