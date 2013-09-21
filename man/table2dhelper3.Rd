\name{table2dhelper3}
\alias{table2dhelper3}
\title{Generates percents from counts returned by the function \code{table2d.ds}}
\usage{
  table2dhelper3(input, valid.output.array,
    zero.studies.valid, unique.categories.1,
    unique.categories.2, num.cats.1, num.cats.2,
    opals.valid.id, num.valid.tables)
}
\arguments{
  \item{input}{a list object returned by the server side
  function \code{table2d.ds}}

  \item{valid.output.array}{a valid array}

  \item{zero.studies.valid}{a boolean indicating if all
  studies were not valid}

  \item{unique.categories.1}{a vector, the full set of
  unique categories across all studies (from valid
  binaries)}

  \item{unique.categories.2}{a vector, the full set of
  unique categories across all studies (from valid
  binaries)}

  \item{num.cats.1}{length of the vector
  'unique.categories.1'}

  \item{num.cats.2}{length of the vector
  'unique.categories.2'}

  \item{opals.valid.id}{a vector that gives the indices of
  the studies with valid tabulated vectors}

  \item{num.valid.tables}{number of studies with valid
  tabulated vectors (no cell with count > 0 and < 5)}
}
\value{
  a list that contains summary tables with columns and rows
  percents
}
\description{
  this is an INTERNAL function required by
  t\code{table2d.ds}. The function is not 'exported' and
  hence not visible to user.
}
\author{
  Burton, P.; Gaye, A.
}

