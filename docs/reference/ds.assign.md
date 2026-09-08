# Assigns an R object to a name in the server-side

This function assigns a datashield object to a name, hence creating a
new object.

## Usage

``` r
ds.assign(toAssign = NULL, newobj = NULL, datasources = NULL)
```

## Arguments

- toAssign:

  a character string providing the object to assign.

- newobj:

  a character string that provides the name for the output object that
  is stored on the data servers. Default `assign.newobj`.

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the `datasources` argument is not
  specified the default set of connections will be used: see
  [`datashield.connections_default`](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

## Value

`ds.assign` returns the R object assigned to a name that is written to
the server-side.

## Details

The new object is stored on the server-side.

`ds.assign` causes a remote assignment by using
[`DSI::datashield.assign`](https://datashield.github.io/DSI/reference/datashield.assign.html).
The `toAssign` argument is checked at the server and assigned the
variable called `newobj` on the server-side.

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
  
  # Assign a variable to a name
  ds.assign(toAssign = "D$LAB_TSC",
            newobj = "labtsc",
            datasources = connections[1]) #only the first Opal server is used ("study1")
                
  # Clear the Datashield R sessions and logout                 
  datashield.logout(connections) 
  
} # }   
```
