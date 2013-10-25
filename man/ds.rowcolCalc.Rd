\name{ds.rowcolCalc}
\alias{ds.rowcolCalc}
\title{Computes sums and means of rows or columns of numeric arrays}
\usage{
  ds.rowcolCalc(datasources = NULL, dataset = NULL,
    operation = NULL, newobj = NULL)
}
\arguments{
  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources.}

  \item{dataset}{an array of two or more dimensions.}

  \item{operation}{a character string which indicates the
  operation to carry out: "rowSums", "colSums", "rowMeans"
  or "colMeans".}

  \item{newobj}{the name of the new object. If this
  argument is set to NULL, the name of the new variable,
  set by default, is 'rowcolCalc_out'.}
}
\value{
  a message is displayed when the action is completed.
}
\description{
  Computes sums and means of rows or columns of numeric
  arrays
}
\details{
  The function is similar to R base functions 'rowSums',
  'colSums', 'rowMeans' and 'colMeans'. It is an assign
  function which means its outcome is not returned to the
  user as it might be revealing if the array contains only
  one row or one column.
}
\examples{
{

# load that contains the login details
data(logindata)

# login and assign the whole dataset on the opal server
opals <- datashield.login(logins=logindata,assign=TRUE)

# create an array using the variables "LAB_HDL" and"LAB_TSC" which we know are both numeric vectors
datashield.assign(opals, "hdl_tsc", quote(data.frame(cbind(D$LAB_HDL, D$LAB_TSC))))

# now turn the newly created vector 'gender_ch' into a numeric
ds.rowcolCalc(datasources=opals, dataset=quote(hdl_tsc), operation="rowSums", newobj="rsum_hdl_tsc")

}
}
\author{
  Gaye, A. (amadou.gaye
}

