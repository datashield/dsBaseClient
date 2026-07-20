# Constructs a list of objects in the server-side

This is similar to the R function `list`.

## Usage

``` r
ds.list(
  x = NULL,
  newobj = NULL,
  datasources = NULL,
  classConsistencyCheck = TRUE
)
```

## Arguments

- x:

  a character string specifying the names of the objects to coerce into
  a list.

- newobj:

  a character string that provides the name for the output variable that
  is stored on the data servers. Default `list.newobj`.

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the `datasources` argument is not
  specified the default set of connections will be used: see
  [`datashield.connections_default`](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

- classConsistencyCheck:

  logical. If TRUE, verifies that each input object has the same class
  across all studies before coercion. Default TRUE.

## Value

`ds.list` returns a list of objects for each study that is stored on the
server-side.

## Details

If the objects to coerce into a list are for example vectors held in a
matrix or a data frame the names of the elements in the list are the
names of columns.

Server function called: `listDS`

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
  
 # combine the 'LAB_TSC' and 'LAB_HDL' variables into a list
 myobjects <- c('D$LAB_TSC', 'D$LAB_HDL')
 ds.list(x = myobjects,
         newobj = "new.list",
         datasources = connections)
  
  # clear the Datashield R sessions and logout
  datashield.logout(connections)
} # }
```
