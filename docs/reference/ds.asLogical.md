# Converts a server-side R object into a logical class

Coerces an R object into a logical class. This function is based on the
native R function `as.logical`.

## Usage

``` r
ds.asLogical(x.name = NULL, newobj = NULL, datasources = NULL)
```

## Arguments

- x.name:

  a character string providing the name of the input object to be
  coerced to a logical.

- newobj:

  a character string that provides the name for the output object that
  is stored on the data servers. Default `aslogical.newobj`.

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the `datasources` argument is not
  specified the default set of connections will be used: see
  [`datashield.connections_default`](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

## Value

`ds.asLogical` returns the R object converted into a logical that is
written to the server-side. Also, two validity messages are returned to
the client-side indicating the name of the `newobj` which has been
created in each data source and if it is in a valid form.

## Details

Server function called: `asLogicalDS`

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
  
  # Converting the R object into a logical
  ds.asLogical(x.name = "D$LAB_TSC", 
               newobj = "logical.obj", 
               datasources =connections[1]) #only the first Opal server is used ("study1")
  ds.class(x = "logical.obj", datasources = connections[1])  
               
  # Clear the Datashield R sessions and logout                 
  datashield.logout(connections) 
  
} # }   
```
