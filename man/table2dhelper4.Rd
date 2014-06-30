\name{table2dhelper4}
\alias{table2dhelper4}
\title{Finalizes the output of the server side function \code{table2dDS}}
\usage{
table2dhelper4(server.func.output, var.name.1, var.name.2)
}
\arguments{
  \item{server.func.output}{a list object returned by the
  server side function \code{table2dDS}}

  \item{var.name.1}{a character string, the name of the
  first input numerical vector}

  \item{var.name.2}{a character string, the name of the
  second input numerical vector}
}
\value{
a list that contains the elements returned by the client
function \code{ds.table2D}
}
\description{
This is an INTERNAL function.
}
\details{


The function is only called by the client function
\code{ds.table2d}. It calls other internal functions namely
\code{table2dhelper1}, \code{table2dhelper2} and
\code{table2dhelper3}.
}
\author{
Burton, P.; Gaye, A.
}
\keyword{internal}

