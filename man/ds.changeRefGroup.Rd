\name{ds.changeRefGroup}
\alias{ds.changeRefGroup}
\title{Changes the reference level of a factor}
\usage{
ds.changeRefGroup(xvect = NULL, ref = NULL, newobj = NULL,
  reorderByRef = FALSE, datasources = NULL)
}
\arguments{
  \item{xvect}{a character, the name of a vector of type
  factor.}

  \item{ref}{the reference level}

  \item{newobj}{the name of the new variable. If this
  argument is set to NULL, the name of the new variable is
  the name of the input variable with the suffixe
  '_newref'.}

  \item{reorderByRef}{a boolean that tells whether or not
  the new vector should be ordered by the reference group
  (i.e. putting the reference group first). The default is
  to not re-order for the reasons explained in the
  'details' section.}

  \item{datasources}{a list of opal object(s) obtained
  after login in to opal servers; these objects hold also
  the data assign to R, as \code{dataframe}, from opal
  datasources.}
}
\value{
nothing is returned to the client, the new object is stored
on the server side.
}
\description{
This function is similar to R function \code{relevel}.
}
\details{


In addition to what the R function does, this function
allows for the user to re-order the vector, putting the
reference group first. If the user chooses the re-order a
warning is issued as this can introduce a mismatch of
values if the vector is put back into a table that is not
reordered in the same way. Such mismatch can render the
results of operations on that table invalid.
}
\examples{
{

# load that contains the login details
data(logindata)

# login and assign all the variables in the opal servers
opals <- datashield.login(logins=logindata,assign=TRUE)

# Example 1: rename the categories and change the reference with re-ordering
# print out the levels of the initial vector
ds.levels('D$PM_BMI_CATEGORICAL')
# define a vector with the new levels and recode the initial levels
newNames <- c('normal', 'overweight', 'obesity')
ds.recodeLevels('D$PM_BMI_CATEGORICAL', newCategories=newNames, newobj='bmi_new')
# print out the levels of the new vector
ds.levels('bmi_new')
# by default the reference is the first level in the vector of levels (here 'normal')
# now change the set the reference to 'obesity' without changing the order (default)
ds.changeRefGroup(xvect='bmi_new', ref='obesity', newobj='bmi_ob')
# print out the levels; the first listed level (i.e. the reference) is now 'obesity'
ds.levels('bmi_ob')

# Example 2: change the reference and re-order by the refence level
# If re-ordering is sought, the action is completed but a warning is issued.
ds.recodeLevels('D$PM_BMI_CATEGORICAL', newCategories=newNames, newobj='bmi_new')
ds.changeRefGroup(xvect='bmi_new', ref='obesity', newobj='bmi_ob', reorderByRef=TRUE)

}
}
\author{
Isaeva, I.; Gaye, A.
}

