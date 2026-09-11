# Converts a server-side R object into a numeric class

Coerces an R object into a numeric class. This function is based on the
native R function `as.numeric`.

## Usage

``` r
ds.asNumeric(x.name = NULL, newobj = NULL, datasources = NULL)
```

## Arguments

- x.name:

  a character string providing the name of the input object to be
  coerced to a numeric.

- newobj:

  a character string that provides the name for the output object that
  is stored on the data servers. Default `asnumeric.newobj`.

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the `datasources` argument is not
  specified the default set of connections will be used: see
  [`datashield.connections_default`](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

## Value

`ds.asNumeric` returns the R object converted into a numeric class that
is written to the server-side.

## Details

This function is based on the native R function `as.numeric`. However,
it behaves differently with some specific classes of variables. For
example, if the input object is of class factor, it first converts its
values into characters and then convert those to numerics. This
behaviour is important for the case where the input object is of class
factor having numbers as levels. In that case, the native R `as.numeric`
function returns the underlying level codes and not the values as
numbers. For example `as.numeric` in R converts the factor vector:  
0 1 1 2 1 0 1 0 2 2 2 1  
Levels: 0 1 2  
to the following numeric vector: 1 2 2 3 2 1 2 1 3 3 3 2  
In contrast DataSHIELD converts an input factor with numeric levels to
its original numeric values.

Server function called: `asNumericDS`

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
  
  # Converting the R object into a numeric class
  ds.asNumeric(x.name = "D$LAB_TSC",
                  newobj = "num.obj",
                  datasources = connections[1]) #only the first Opal server is used ("study1")
  ds.class(x = "num.obj", datasources = connections[1]) 
                
  # Clear the Datashield R sessions and logout                 
  datashield.logout(connections) 
  
} # }   
```
