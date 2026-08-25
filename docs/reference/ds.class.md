# Class of the R object in the server-side

Retrieves the class of an R object. This function is similar to the R
function `class`.

## Usage

``` r
ds.class(x = NULL, datasources = NULL)
```

## Arguments

- x:

  a character string providing the name of the input R object.

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the `datasources` argument is not
  specified the default set of connections will be used: see
  [`datashield.connections_default`](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

## Value

`ds.class` returns the type of the R object.

## Details

Same as the native R function `class`.

Server function called: `classDS`

## See also

[`ds.exists`](ds.exists.md) to verify if an object is defined (exists)
on the server-side.

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

  # Getting the class of the R objects stored in the server-side
  ds.class(x = "D", #whole dataset
           datasources = connections[1]) #only the first server ("study1") is used

  ds.class(x = "D$LAB_TSC", #select a variable
           datasources = connections[1]) #only the first server ("study1") is used
           
  # Clear the Datashield R sessions and logout
  datashield.logout(connections) 
} # }
```
