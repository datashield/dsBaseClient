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
    \item{\code{user}}{a character, a formal username or a 
          path to a valid ssl certificate, if required}
    \item{\code{password}}{a character, a formal password or a 
          path to a valid ssl key if required}
    \item{\code{table}}{a character, the path to the opal datasource that holds the data
         to analyse}
  }
}

\examples{data(logindata)}
