# Perform 'unique' on a variable on the server-side

Perform 'unique', from the 'base' package on a specified variable on the
server-side

## Usage

``` r
ds.unique(x.name = NULL, newobj = NULL, datasources = NULL)
```

## Arguments

- x.name:

  a character string providing the name of the variable, in the server,
  to perform `unique` upon

- newobj:

  a character string that provides the name for the output object that
  is stored on the data servers. Default `unique.newobj`.

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the `datasources` argument is not
  specified the default set of connections will be used: see
  [`datashield.connections_default`](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

## Value

`ds.unique` returns the vector of unique R objects which are written to
the server-side.

## Details

Will create a vector or list which has no duplicate values.

Server function called: `uniqueDS`

## Author

Stuart Wheater, DataSHIELD Development Team

Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands

## Examples

``` r
if (FALSE) { # \dontrun{
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

  # Create a vector with combined objects
  ds.unique(x.name = "D$LAB_TSC", newobj = "new.vect", datasources = connections)

  # Clear the Datashield R sessions and logout
  datashield.logout(connections)
} # }
```
