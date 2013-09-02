\name{logindata}

\alias{logindata}

\docType{data}

\title{Information required to login to opal servers}

\description{
    A table of with 5 columns: study name, URL, username, password and opal datasource.
}

\usage{data(logindata)}

\format{
  A data frame where the number of servers corresponds to the number of rows
  \describe{
    \item{\code{server}}{a character, the formal name of the study}
    \item{\code{url}}{URL of the opal server}
    \item{\code{userID}}{login username}
    \item{\code{pwd}}{login pwd}
    \item{\code{opalPath}}{path to the opal datasource where the data is stored}
  }
}

\examples{data(logindata)}
