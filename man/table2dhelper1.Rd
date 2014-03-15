\name{table2dhelper1}
\alias{table2dhelper1}
\title{Checks validity of 2D-tables}
\usage{
table2dhelper1(input, varname1, varname2)
}
\arguments{
  \item{input}{a list object returned by the server side
  function \code{table2d.ds}}

  \item{varname1}{a character string, the name of the first
  input numerical vector}

  \item{varname2}{a character string, the name of the
  second input numerical vector}
}
\value{
a list that contains the following elements:
\code{num.opals} the number of studies to analyse
\code{num.valid.tables} number of studies with valid
tabulated vectors (no cell with count > 0 and < 5)
\code{opals.valid.binary} vector of binaries indicating
whether or not the tabulate vector was valid or not (0 =
not valid and 1 = not valid) \code{opals.valid.id} a vector
that gives the indices of the studies with valid tabulated
vectors \code{zero.studies.valid} a boolean indicating if
all studies were not valid, 'TRUE' if none of the studies
is valid, 'FALSE' otherwise
}
\description{
this is an INTERNAL function required by the client
function \code{ds.table2d}. It verifies the cell counts of
the tabulate vectors which are considered valid if no count
> 0 and < 5 and not valid otherwise.
}
\details{


the function is not 'exported' and hence not visible to
user and is only called by \code{ds.table2d}
}
\author{
Burton, P.; Gaye, A.
}

