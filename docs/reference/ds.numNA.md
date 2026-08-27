# Gets the number of missing values in a server-side vector

This function helps to know the number of missing values in a vector
that is stored on the server-side.

## Usage

``` r
ds.numNA(x = NULL, datasources = NULL, classConsistencyCheck = TRUE)
```

## Arguments

- x:

  a character string specifying the name of the vector.

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the `datasources` argument is not
  specified the default set of connections will be used: see
  [`datashield.connections_default`](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

- classConsistencyCheck:

  logical. If TRUE, checks that the input object has the same class
  across all studies. Default TRUE.

## Value

`ds.numNA` returns to the client-side the number of missing values on a
server-side vector.

## Details

The number of missing entries are counted and the total for each study
is returned.

Server function called: `numNaDS`

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
  
  #Get the number of missing values on a server-side vector 
  
  ds.numNA(x = "D$LAB_TSC",
           datasources = connections)
  
  # clear the Datashield R sessions and logout
  datashield.logout(connections)


} # }
```
