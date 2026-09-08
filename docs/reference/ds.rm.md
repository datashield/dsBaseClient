# Deletes server-side R objects

deletes R objects on the server-side

## Usage

``` r
ds.rm(x.names = NULL, datasources = NULL)
```

## Arguments

- x.names:

  a character string specifying the objects to be deleted.

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the `datasources` argument is not
  specified the default set of connections will be used: see
  [`datashield.connections_default`](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

## Value

The `ds.rm` function deletes from the server-side the specified object.
If this is successful the message `"Object(s) '<x.names>' was deleted."`
is returned to the client-side.

## Details

This function is similar to the native R function
[`rm()`](https://rdrr.io/r/base/rm.html).

The fact that it is an aggregate function may be surprising because it
modifies an object on the server-side, and would, therefore, be expected
to be an assign function. However, as an assign function the last step
in running it would be to write the modified object as `newobj`. But
this would fail because the effect of the function is to delete the
object and so it would be impossible to write it anywhere. Please note
that although this calls an aggregate function there is no `type`
argument.

Server function called: `rmDS`

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
  
  #Create an object in the server-side
  
  ds.assign(toAssign = "D$LAB_TSC",
            newobj = "labtsc",
            datasources = connections)
  
  #Delete "labtsc" object from the server-side
  
  ds.rm(x.names = "labtsc",
        datasources = connections)
             
  # clear the Datashield R sessions and logout
  datashield.logout(connections)
} # }
```
