\name{table2dhelper4}
\alias{table2dhelper4}
\title{Finalizes the output of the function \code{table2d.ds}}
\usage{
  table2dhelper4(server.func.output, var.name.1,
    var.name.2)
}
\arguments{
  \item{server.func.output}{a list object returned by the
  server side function \code{table2d.ds}}

  \item{var.name.1}{a character string, the name of the
  first input numerical vector}

  \item{var.name.2}{a character string, the name of the
  second input numerical vector}
}
\value{
  a list that contains the elements returned by the client
  function \code{ds.table2d}
}
\description{
  This is an INTERNAL i.e. it is not 'exported' and hence
  not visible to users. This function is only called by the
  client function \code{ds.table2d}. It calls other
  internal functions namely \code{table2dhelper1},
  \code{table2dhelper2} and \code{table2dhelper3}.
}
\author{
  Burton, P.; Gaye, A.
}

