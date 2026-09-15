# Lists server-side functions

Lists all current server-side functions

## Usage

``` r
ds.listServersideFunctions(datasources = NULL)
```

## Arguments

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the `datasources` argument is not
  specified the default set of connections will be used: see
  [`datashield.connections_default`](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

## Value

`ds.listServersideFunctions` returns to the client-side a list
containing all server-side functions separately for each study. Firstly
lists assign and then aggregate functions.

## Details

Uses
[`datashield.methods`](https://datashield.github.io/DSI/reference/datashield.methods.html)
function from `DSI` package to list all assign and aggregate functions
on the available data repository servers. The only choice of arguments
is in `datasources`; i.e. which studies to interrogate. Once the studies
have been selected `ds.listServersideFunctions` lists all assign
functions for all of these studies and then all aggregate functions for
all of them.

This function does not call any server-side function.

## Author

DataSHIELD Development Team

## Examples

``` r
if (FALSE) { # \dontrun{
 
  ## Version 6, for version 5 see Wiki
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
  
  # List server-side functions
  
  ds.listServersideFunctions(datasources = connections)
            
  # Clear the Datashield R sessions and logout  
  datashield.logout(connections) 
} # }
```
