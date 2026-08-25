# Computes the quantiles of a server-side variable

This function calculates the mean and quantile values of a server-side
quantitative variable.

## Usage

``` r
ds.quantileMean(x = NULL, type = "combine", datasources = NULL)
```

## Arguments

- x:

  a character string specifying the name of the numeric vector.

- type:

  a character that represents the type of graph to display. This can be
  set as `'combine'` or `'split'`. For more information see **Details**.

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the `datasources` argument is not
  specified the default set of connections will be used: see
  [`datashield.connections_default`](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

## Value

`ds.quantileMean` returns to the client-side the quantiles and
statistical mean of a server-side numeric vector.

## Details

This function does not return the minimum and maximum values because
they are potentially disclosive.

Depending on the argument `type` can be carried out two types of
analysis:  
(1) `type = 'combine'` pooled values are displayed  
(2) `type = 'split'` summaries are returned for each study.

Server functions called: `quantileMeanDS`, `length` and `numNaDS`

## See also

[`ds.mean`](ds.mean.md) to compute the statistical mean.

[`ds.summary`](ds.summary.md) to generate the summary of a variable.

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
  
  #Get the quantiles and mean of a server-side variable
  
  ds.quantileMean(x = "D$LAB_TRIG",
                  type = "combine",
                  datasources = connections)
  
  
  # clear the Datashield R sessions and logout
  datashield.logout(connections)


} # }
```
