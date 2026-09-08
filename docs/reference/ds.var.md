# Computes server-side vector variance

Computes the variance of a given server-side vector.

## Usage

``` r
ds.var(
  x = NULL,
  type = "split",
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

`ds.var` returns to the client-side a list including:  

`Variance.by.Study`: estimated variance, `Nmissing` (number of missing
observations), `Nvalid` (number of valid observations) and `Ntotal` (sum
of missing and valid observations) separately for each study (if
`type = split` or `type = both`).  
`Global.Variance`: estimated variance, `Nmissing`, `Nvalid` and `Ntotal`
across all studies combined (if `type = combine` or `type = both`).  
`Nstudies`: number of studies being analysed.  

## Details

This function is similar to the R function `var`.

The function can carry out 3 types of analysis depending on the argument
`type`:  
(1) If `type` is set to `'combine'`, `'combined'`, `'combines'` or
`'c'`, a global variance is calculated.  
(2) If `type` is set to `'split'`, `'splits'` or `'s'`, the variance is
calculated separately for each study.  
(3) If `type` is set to `'both'` or `'b'`, both sets of outputs are
produced.

Server function called: `varDS`

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
  
  #Calculate the variance of a vector in the server-side
  
  ds.var(x = "D$LAB_TSC",
          type = "split",
          datasources = connections)
             
  # clear the Datashield R sessions and logout
  datashield.logout(connections)
} # }
```
