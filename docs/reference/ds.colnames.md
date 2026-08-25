# Produces column names of the R object in the server-side

Retrieves column names of an R object on the server-side. This function
is similar to R function `colnames`.

## Usage

``` r
ds.colnames(x = NULL, datasources = NULL)
```

## Arguments

- x:

  a character string providing the name of the input data frame or
  matrix.

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the `datasources` argument is not
  specified the default set of connections will be used: see
  [`datashield.connections_default`](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

## Value

`ds.colnames` returns the column names of the specified server-side data
frame or matrix.

## Details

The input is restricted to the object of type `data.frame` or `matrix`.

Server function called: `colnamesDS`

## See also

[`ds.dim`](ds.dim.md) to obtain the dimensions of a matrix or a data
frame.

## Author

DataSHIELD Development Team

## Examples

``` r
if (FALSE) { # \dontrun{

  ## Version 6, for version 5 see the Wiki
  # Connecting to the Opal servers

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
  
  # Log onto the remote Opal training servers
  connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D") 

  # Getting column names of the R objects stored in the server-side
  ds.colnames(x = "D",
              datasources = connections[1]) #only the first server ("study1") is used
  # Clear the Datashield R sessions and logout
  datashield.logout(connections) 
} # }
```
