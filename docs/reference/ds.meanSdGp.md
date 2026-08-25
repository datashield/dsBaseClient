# Computes the mean and standard deviation across groups defined by one factor

This function calculates the mean and SD of a continuous variable for
each class of a single factor.

## Usage

``` r
ds.meanSdGp(
  x = NULL,
  y = NULL,
  type = "both",
  do.checks = FALSE,
  datasources = NULL
)
```

## Arguments

- x:

  a character string specifying the name of a numeric continuous
  variable.

- y:

  a character string specifying the name of a categorical variable of
  class factor.

- type:

  a character string that represents the type of analysis to carry out.
  This can be set as: `"combine"`, `"split"` or `"both"`. Default
  `"both"`. For more information see **Details**.

- do.checks:

  logical. If TRUE the administrative checks are undertaken to ensure
  that the input objects are defined in all studies and that the
  variables are of equivalent class in each study. Default is FALSE to
  save time.

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the `datasources` argument is not
  specified the default set of connections will be used: see
  [`datashield.connections_default`](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

## Value

`ds.meanSdGp` returns to the client-side the mean, SD, Nvalid and SEM
combined across studies and/or separately for each study, depending on
the argument `type`.

## Details

This function calculates the mean, standard deviation (SD), N (number of
observations) and the standard error of the mean (SEM) of a continuous
variable broken down into subgroups defined by a single factor.

There are important differences between `ds.meanSdGp` function compared
to the function `ds.meanByClass`:

\(A\) `ds.meanSdGp` does not actually subset the data it simply
calculates the required statistics and reports them. This means you
cannot use this function if you wish to physically break the data into
subsets. On the other hand, it makes the function very much faster than
`ds.meanByClass` if you do not need to create physical subsets.  
(B) `ds.meanByClass` allows you to specify up to three categorising
factors, but `ds.meanSdGp` only allows one. However, this is not a
serious problem. If you have two factors (e.g. sex with two levels
`[0,1]` and `BMI.categorical` with three levels `[1,2,3]`) you simply
need to create a new factor that combines the two together in a way that
gives each combination of levels a different value in the new factor.
So, in the example given, the calculation `newfactor = (3*sex) + BMI`
gives you six values:  
(1) `sex = 0` and `BMI = 1` -\> `newfactor = 1`  
(2) `sex = 0` and `BMI = 2` -\> `newfactor = 2`  
(3) `sex = 0` and `BMI = 3` -\> `newfactor = 3`  
(4) `sex = 1` and `BMI = 1` -\> `newfactor = 4`  
(5) `sex = 1` and `BMI = 2` -\> `newfactor = 5`  
(6) `sex = 1` and `BMI = 3` -\> `newfactor = 6`  

\(C\) At present, `ds.meanByClass` calculates the sample size in each
group to mean the total sample size (i.e. it includes all observations
in each group regardless of whether or not they include missing values
for the continuous variable or the factor). The calculation of sample
size in each group by `ds.meanSdGp` always reports the number of
observations that are non-missing both for the continuous variable and
the factor. This makes sense - in the case of `ds.meanByClass`, the
total size of the physical subsets was important, but when it comes down
only to `ds.meanSdGp` which undertakes analysis without physical
subsetting, it is only the observations with non-missing values in both
variables that contribute to the calculation of means and SDs within
each group and so it is logical to consider those counts as primary. The
only reference `ds.meanSdGp` makes to missing counts is in the reporting
of `Ntotal` and `Nmissing` overall (ie not broken down by group).

For the future, we plan to extend `ds.meanByClass` to report both total
and non-missing counts in subgroups.

Depending on the variable `type` can be carried out different
analysis:  
(1) `"combine"`: a pooled table of results is generated.  
(2) `"split"` a table of results is generated for each study.  
(3) `"both"` both sets of outputs are produced.

Server function called: `meanSdGpDS`

## Author

DataSHIELD Development Team

## Examples

``` r
if (FALSE) { # \dontrun{

 ## Version 6, for version 5 see the Wiki
  
  # connecting to the Opal servers

  require('DSI')
  require('DSOpal')
  require('dsBaseClient')

  builder <- DSI::newDSLoginBuilder()
  builder$append(server = "study1", 
                 url = "http://192.168.56.100:8080/", 
                 user = "administrator", password = "datashield_test&", 
                 table = "SURVIVAL.EXPAND_NO_MISSING1", driver = "OpalDriver")
  builder$append(server = "study2", 
                 url = "http://192.168.56.100:8080/", 
                 user = "administrator", password = "datashield_test&", 
                 table = "SURVIVAL.EXPAND_NO_MISSING2", driver = "OpalDriver")
  builder$append(server = "study3",
                 url = "http://192.168.56.100:8080/", 
                 user = "administrator", password = "datashield_test&", 
                 table = "SURVIVAL.EXPAND_NO_MISSING3", driver = "OpalDriver")
  logindata <- builder$build()
  
  connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D") 


  #Example 1: Calculate the mean, SD, Nvalid and SEM of the continuous variable age.60 (age in
  #years centralised at 60), broken down by time.id (a six level factor relating to survival time)
  #and report the pooled results combined across studies.
 
  ds.meanSdGp(x = "D$age.60",
              y = "D$time.id",
              type = "combine",
              do.checks = FALSE,
              datasources = connections)
              
  #Example 2: Calculate the mean, SD, Nvalid and SEM of the continuous variable age.60 (age in
  #years centralised at 60), broken down by time.id (a six level factor relating to survival time)
  #and report both study-specific results and the pooled results combined across studies.
  #Save the returned output to msg.b.
  
  ds.meanSdGp(x = "D$age.60",
              y = "D$time.id",
              type = "both",
              do.checks = FALSE,
              datasources = connections)  
             
  # clear the Datashield R sessions and logout
  datashield.logout(connections)
} # }
```
