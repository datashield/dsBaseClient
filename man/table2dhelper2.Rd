\name{table2dhelper2}
\alias{table2dhelper2}
\title{Generates a valid array}
\usage{
  table2dhelper2(input, num.valid.tables,
    opals.valid.binary, opals.valid.id, zero.studies.valid)
}
\arguments{
  \item{input}{a list object returned by the server side
  function \code{table2d.ds}}

  \item{num.valid.tables}{number of studies with valid
  tabulated vectors (no cell with count > 0 and < 5)}

  \item{opals.valid.binary}{vector of binaries indicating
  whether or not the tabulate vector was valid or not (0 =
  not valid and 1 = not valid)}

  \item{opals.valid.id}{a vector that gives the indices of
  the studies with valid tabulated vectors}

  \item{zero.studies.valid}{a boolean indicating if all
  studies were not valid}
}
\value{
  a list which contains a valid array and vectors of unique
  categories
}
\description{
  this is an INTERNAL function required by the client
  function \code{ds.table2d}. The function is not
  'exported' and hence not visible to user and is only
  called by \code{ds.table2d}
}
\author{
  Burton, P.; Gaye, A.
}

