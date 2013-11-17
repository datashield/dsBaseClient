\name{propMeanHelper}
\alias{propMeanHelper}
\title{Runs a combined GLM analysis of non-pooled data}
\usage{
  propMeanHelper(datasources = NULL, formula = NULL,
    family = NULL, maxit = 30)
}
\arguments{
  \item{datasources}{a list of opal object(s) obtained
  after login to opal servers; these objects also hold the
  data assigned to R, as a \code{dataframe}, from opal
  datasources.}

  \item{formula}{an object of class \code{formula} which
  describes the model to be fitted}

  \item{family}{a description of the error distribution
  function to use in the model}

  \item{maxit}{the number of iterations of IWLS used}
}
\value{
  everything that is returned by tthe function 'ds.glm' in
  the package 'dsmodellingclient'
}
\description{
  THIS IS JUST THE FUNCTION 'ds.glm' WITHOUT THE CHECKS AT
  THE TOP. THE LONG NAME RESULTING FROM SEVERAL SUBSETTING
  IN 'ds.propMean' SCREWS THOSE CHECKS. SO INSTEAD OF
  ALTERING 'ds.glm' TO FIT THE REQUIREMENTS OF THIS
  FUNCTION IT IS BETTER AND EASIER TO JUST REPLICATE THE
  GLM WITHOUT THE CHECKS WITH A DIFFERENT NAME THAT DOES
  NOT MATTER AS IT IS HERE AS AN INTERNAL FUNCTION, NOT
  EXPORTED, AND CALLED ONLY BY THE FUNCTION 'ds.propMean'.
}

