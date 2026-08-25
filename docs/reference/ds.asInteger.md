# Converts a server-side R object into an integer class

Coerces an R object into an integer class. This function is based on the
native R function `as.integer`.

## Usage

``` r
ds.asInteger(x.name = NULL, newobj = NULL, datasources = NULL)
```

## Arguments

- x.name:

  a character string providing the name of the input object to be
  coerced to an integer.

- newobj:

  a character string that provides the name for the output object that
  is stored on the data servers. Default `asinteger.newobj`.

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the `datasources` argument is not
  specified the default set of connections will be used: see
  [`datashield.connections_default`](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

## Value

`ds.asInteger` returns the R object converted into an integer that is
written to the server-side. Also, two validity messages are returned to
the client-side indicating the name of the `newobj` which has been
created in each data source and if it is in a valid form.

## Details

This function is based on the native R function `as.integer`. The only
difference is that the DataSHIELD function first converts the values of
the input object into characters and then convert those to integers.
This addition, it is important for the case where the input object is of
class factor having integers as levels. In that case, the native R
`as.integer` function returns the underlying level codes and not the
values as integers. For example `as.integer` in R converts the factor
vector:  
\[1\] 0 1 1 2 1 0 1 0 2 2 2 1  
Levels: 0 1 2  
to the following integer vector: 1 2 2 3 2 1 2 1 3 3 3 2  

Server function called: `asIntegerDS`

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
  
  # Converting the R object into an integer
  ds.asInteger(x.name = "D$LAB_TSC",
                  newobj = "int.obj",
                  datasources = connections[1]) #only the first Opal server is used ("study1")
  ds.class(x = "int.obj", datasources = connections[1])   
  
  # Clear the Datashield R sessions and logout                 
  datashield.logout(connections) 
  
} # }   
```
