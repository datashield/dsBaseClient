# Computes server-side vector statistical mean

This function computes the statistical mean of a given server-side
vector.

## Usage

``` r
ds.mean(
  x = NULL,
  type = "split",
  save.mean.Nvalid = FALSE,
  datasources = NULL,
  classConsistencyCheck = FALSE
)
```

## Arguments

- x:

  a character specifying the name of a numerical vector.

- type:

  a character string that represents the type of analysis to carry out.
  This can be set as `'combine'`, `'combined'`, `'combines'`, `'split'`,
  `'splits'`, `'s'`, `'both'` or `'b'`. For more information see
  **Details**.

- save.mean.Nvalid:

  logical. If TRUE generated values of the mean and the number of valid
  (non-missing) observations will be saved on the data servers. Default
  FALSE. For more information see **Details**.

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the `datasources` argument is not
  specified the default set of connections will be used: see
  [`datashield.connections_default`](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

- classConsistencyCheck:

  logical. If TRUE, checks that the input object has the same class
  across all studies. Default FALSE.

## Value

`ds.mean` returns to the client-side a list including:  

`Mean.by.Study`: estimated mean, `Nmissing` (number of missing
observations), `Nvalid` (number of valid observations) and `Ntotal` (sum
of missing and valid observations) separately for each study (if
`type = split` or `type = both`).  
`Global.Mean`: estimated mean, `Nmissing`, `Nvalid` and `Ntotal` across
all studies combined (if `type = combine` or `type = both`).  
`Nstudies`: number of studies being analysed.  

If `save.mean.Nvalid` is set as TRUE, the objects `Nvalid.all.studies`,
`Nvalid.study.specific`, `mean.all.studies` and `mean.study.specific`
are written to the server-side.

## Details

This function is similar to the R function `mean`.

The function can carry out 3 types of analysis depending on the argument
`type`:  
(1) If `type` is set to `'combine'`, `'combined'`, `'combines'` or
`'c'`, a global mean is calculated.  
(2) If `type` is set to `'split'`, `'splits'` or `'s'`, the mean is
calculated separately for each study.  
(3) If `type` is set to `'both'` or `'b'`, both sets of outputs are
produced.

If the argument `save.mean.Nvalid` is set to TRUE study-specific means
and `Nvalids` as well as the global equivalents across all studies
combined are saved in the server-side. Once the estimated means and
`Nvalids` are written into the server-side R environments, they can be
used directly to centralize the variable of interest around its global
mean or its study-specific means. Finally, the `isDefined` internal
function checks whether the key variables have been created.

Server function called: `meanDS`

## See also

`ds.quantileMean` to compute quantiles.

`ds.summary` to generate the summary of a variable.

## Author

DataSHIELD Development Team

Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands

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
                 table = "CNSIM.CNSIM1", driver = "OpalDriver")
  builder$append(server = "study2", 
                 url = "http://192.168.56.100:8080/", 
                 user = "administrator", password = "datashield_test&", 
                 table = "CNSIM.CNSIM2", driver = "OpalDriver")
  builder$append(server = "study3",
                 url = "http://192.168.56.100:8080/", 
                 user = "administrator", password = "datashield_test&", 
                 table = "CNSIM.CNSIM3", driver = "OpalDriver")
  logindata <- builder$build()
  
  connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D") 
  
  #Calculate the mean of a vector in the server-side
  
  ds.mean(x = "D$LAB_TSC",
          type = "split",
          save.mean.Nvalid = FALSE,
          datasources = connections)
             
  # clear the Datashield R sessions and logout
  datashield.logout(connections)
} # }
```
