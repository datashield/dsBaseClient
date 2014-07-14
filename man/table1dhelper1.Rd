\name{table1dhelper1}
\alias{table1dhelper1}
\title{Checks validity of tabulated factor variable}
\usage{
table1dhelper1(input)
}
\arguments{
  \item{input}{a list object returned by the server side
  function \code{table1dDS}}
}
\value{
a list that contains the following elements:
\code{num.opals} the number of studies to analyse
\code{num.valid.tables} number of studies with valid
tabulated vectors (no cell with count > 0 and < filter)
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
function \code{ds.table1D}.
}
\details{


It verifies the cell counts of the tabulate vectors which
are considered valid if no count > 0 and < filter and not
valid otherwise.
}
\author{
Burton, P.; Gaye, A.
}
\keyword{internal}

