# Tests for correlation between paired samples in the server-side

This is similar to the R stats function `cor.test`.

## Usage

``` r
ds.corTest(
  x = NULL,
  y = NULL,
  method = "pearson",
  exact = NULL,
  conf.level = 0.95,
  type = "split",
  datasources = NULL,
  classConsistencyCheck = FALSE
)
```

## Arguments

- x:

  a character string providing the name of a numerical vector.

- y:

  a character string providing the name of a numerical vector.

- method:

  a character string indicating which correlation coefficient is to be
  used for the test. One of "pearson", "kendall", or "spearman", can be
  abbreviated. Default is set to "pearson".

- exact:

  a logical indicating whether an exact p-value should be computed. Used
  for Kendall's tau and Spearman's rho. See *Details* of R stats
  function `cor.test` for the meaning of NULL (the default).

- conf.level:

  confidence level for the returned confidence interval. Currently only
  used for the Pearson product moment correlation coefficient if there
  are at least 4 complete pairs of observations. Default is set to 0.95.

- type:

  a character string that represents the type of analysis to carry out.
  This must be set to `'split'` or `'combine'`. Default is set to
  `'split'`. If `type` is set to "combine" then an approximated pooled
  correlation is estimated based on Fisher's z transformation.

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

`ds.corTest` returns to the client-side the results of the correlation
test.

## Details

Runs a two-sided correlation test between paired samples, using one of
Pearson's product moment correlation coefficient, Kendall's tau or
Spearman's rho. Server function called: `corTestDS`

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
  
  # test for correlation
  ds.corTest(x = "D$LAB_TSC",
             y = "D$LAB_HDL",
             datasources = connections[1]) #Only first server is used ("study1")
                
  # Clear the Datashield R sessions and logout                 
  datashield.logout(connections) 
  
} # }   
```
