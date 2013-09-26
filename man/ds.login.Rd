\name{ds.login}
\alias{ds.login}
\title{Logs in and assigns variables to R}
\usage{
  ds.login(realdata = FALSE, logins = NULL, assign = FALSE,
    variables = NULL, certificate = NULL, key = NULL)
}
\arguments{
  \item{realdata}{a boolean that indicates if the user is
  carrying out a datashield analysis using real data held
  behind the firewalls of collaborating studies/cohorts.
  The default value is FALSE i.e. the user is not using
  real data rather he/she is using some test data (e.g.
  simulated data).}

  \item{logins}{a dataframe that holds login details}

  \item{assign}{a boolean which tells whether or not data
  should from the opal datasource to R after login into the
  server(s).}

  \item{variables}{specific variables to assign. If
  \code{assign} is set to FALSE this argument is ignored
  otherwise the specified variables are assign to R. If no
  variables are specified (default) the whole dataset is
  assigned.}

  \item{certificate}{a character string, the path to a
  valid datashield ssl certificate, required to run an
  analysis using real data (as opposed to test/simulated
  data). The certificate is required to log into to the
  server that holds the real data behind the firewall of a
  collaborating study.}

  \item{key}{a character string, the path to a valid
  datashield ssl key, required to run an analysis using
  real data (as opposed to test/simulated data). The key is
  required to log into to the server that holds the real
  data behind the firewall of a collaborating study.}
}
\value{
  object(s) of class opal
}
\description{
  This function allows for clients to login to opal servers
  and (optionaly) assign all the data or specific variables
  from Opal datasources to R. The assigned dataframes (one
  for each opal server) are named 'D'. The function can be
  uses to run analysis using test/simulated data or real
  data sitting behind the firewall of a study/cohort.
  Specific credentials including valid datashield
  certificate and key are required to run a real data
  analysis. To obtain the necessary credentials the user
  must contact the studies/cohort whose data he want to use
  and the DataSHIELD development team.
}
\examples{
{

#### The below examples illustrate an analysises that use test/simulated data ####

# load that contains the login details
data(logindata)

# Example 1: just login (default)
opals <- ds.login(logins=logindata)

# Example 2: login and assign the whole dataset
opals <- ds.login(logins=logindata,assign=TRUE)

# Example 3: login and assign specific variable(s)
myvar <- list("LAB_TSC")
opals <- ds.login(logins=logindata,assign=TRUE,variables=myvar)
}
}
\author{
  Gaye, A.
}

