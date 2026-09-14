# Converts a server-side R object into a list

Coerces an R object into a list. This function is based on the native R
function `as.list`.

## Usage

``` r
ds.asList(x.name = NULL, newobj = NULL, datasources = NULL)
```

## Arguments

- x.name:

  a character string providing the name of the input object to be
  coerced to a list.

- newobj:

  a character string that provides the name for the output object that
  is stored on the data servers. Default `aslist.newobj`.

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the `datasources` argument is not
  specified the default set of connections will be used: see
  [`datashield.connections_default`](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

## Value

`ds.asList` returns the R object converted into a list which is written
to the server-side.

## Details

Server function called: `asListDS`

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
  
  # Converting the R object into a List
  ds.asList(x.name = "D",
  newobj = "D.asList", 
  datasources = connections[1]) #only the first Opal server is used ("study1")
  ds.class(x = "D.asList", datasources = connections[1])   
              
  # Clear the Datashield R sessions and logout                 
  datashield.logout(connections) 
  
} # }   
```
