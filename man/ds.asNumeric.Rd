\name{ds.asNumeric}
\alias{ds.asNumeric}
\title{Turns a vector into numerical type}
\usage{
ds.asNumeric(datasources = NULL, xvect = NULL, newobj = NULL)
}
\arguments{
  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources.}

  \item{xvect}{a vector.}

  \item{newobj}{the name of the new variable. If this
  argument is set to NULL, the name of the new variable is
  the name of the input variable with the suffixe '_num'
  (e.g. 'GENDER_num', if input variable's name is 'GENDER)}
}
\value{
a message is displayed when the action is completed.
}
\description{
This function is similar to R function \code{as.numeric}.
It is important to NOTE THAT FACTOR SHOULD BE TURNED INTO
'character' first AND THEN ONLY INTO 'numeric'.
}
\details{

}
\examples{
{

# load that contains the login details
data(logindata)

# login and assign specific variable(s)
myvar <- list("GENDER")
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

# turn the factor variable 'GENDER' into a character vector
ds.asCharacter(datasources=opals, xvect=quote(D$GENDER), newobj="gender_ch")

# now turn the newly created vector 'gender_ch' into a numeric
ds.asNumeric(datasources=opals, xvect=quote(D$GENDER), newobj="gender_nm")
}
}
\author{
Gaye, A.
}

