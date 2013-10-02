\name{ds.t.test.formula}
\alias{ds.t.test.formula}
\title{Runs a student's t-test on horizontally partitioned data (formula method)}
\usage{
  ds.t.test.formula(datasources, formula, data = NULL,
    subset = NULL, na.action = getOption("na.action"),
    type = "combine", alternative = "two.sided", mu = 0,
    paired = FALSE, var.equal = FALSE, conf.level = 0.95)
}
\arguments{
  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources.}

  \item{formula}{a formula of the form y ~ x where y is a
  numeric variable giving the data values and x a factor
  with two levels giving the corresponding groups.}

  \item{data}{an optional matrix or data frame containing
  the variables in the formula formula. By default the
  variables are taken from environment(formula)}

  \item{subset}{an optional vector specifying a subset of
  observations to be used.}

  \item{na.action}{a function which indicates what should
  happen when the data contain NAs. Defaults to
  getOption("na.action")}

  \item{type}{a character which tells if the test is ran
  for the pooled data or not. By default \code{type} is set
  to 'combine' and a t.test of the pooled data is carried
  out. If \code{type} is set to 'split', a t.test is ran
  for each study separately.}

  \item{alternative}{character string specifying the
  alternative hypothesis, must be one of "two.sided"
  (default), "greater" or "less". You can specify just the
  initial letter.}

  \item{mu}{a number indicating the true value of the mean
  (or difference in means if you are performing a two
  sample test).}

  \item{paired}{a logical indicating whether you want a
  paired t-test.}

  \item{var.equal}{a logical variable indicating whether to
  treat the two variances as being equal. If TRUE then the
  pooled variance is used to estimate the variance
  otherwise the Welch. (or Satterthwaite) approximation to
  the degrees of freedom is used.}

  \item{conf.level}{confidence level of the interval}
}
\value{
  a list containing the following elements:
  \code{statistic} the value of the t-statistic
  \code{parameter} the degrees of freedom for the
  t-statistic \code{p.value} p.value the p-value for the
  test \code{conf.int} a confidence interval for the mean
  appropriate to the specified alternative hypothesis
  \code{estimate} the estimated mean or difference in means
  depending on whether it was a one-sample test or a
  two-sample test \code{null.value} the specified
  hypothesized value of the mean or mean difference
  depending on whether it was a one-sample test or a
  two-sample test \code{alternative} a character string
  describing the alternative hypothesis \code{method} a
  character string indicating what type of t-test was
  performed
}
\description{
  Performs two sample t-tests on vectors of data.
}
\details{
  Summary statistics are obtained from each of the data
  sets that are located on the distinct computers/servers.
  And then grand means and variances are calculated. Those
  are used for performing t-test.
}
\examples{
\dontrun{
# load that contains the login details
data(logindata)

# login and assign specific variable(s)
myvar <- list("LAB_TSC", "GENDER")
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# Example 1: Run a t.test of the pooled data for the variables 'LAB_HDL' and 'LAB_TSC' - default
ds.t.test.formula(datasources=opals, formula=D$LAB_TSC~D$GENDER, type="combine")

# Example 2: Run a t.test for each study separately for the same variables as above
ds.t.test(datasources=opals, x=quote(D$LAB_HDL), y=quote(D$LAB_TSC), type="split")
}
}
\author{
  Isaeva, J.; Gaye, A.
}

