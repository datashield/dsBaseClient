# Produces levels attributes of a server-side factor

This function provides access to the level attribute of a factor
variable stored on the server-side. This function is similar to R
function `levels`.

## Usage

``` r
ds.levels(x = NULL, datasources = NULL)
```

## Arguments

- x:

  a character string specifying the name of a factor variable.

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the `datasources` argument is not
  specified the default set of connections will be used: see
  [`datashield.connections_default`](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

## Value

`ds.levels` returns to the client-side the levels of a factor class
variable stored in the server-side.

## Details

Server function called: `levelsDS`

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
  
  # Example 1: Get the levels of the PM_BMI_CATEGORICAL variable
  ds.levels(x = 'D$PM_BMI_CATEGORICAL',
            datasources = connections)#all servers are used
  ds.levels(x = 'D$PM_BMI_CATEGORICAL',
            datasources = connections[2])#only the second server is used (study2)

  # Example 2: Get the levels of the LAB_TSC variable
  # This example should not work because LAB_TSC is a continuous variable
  ds.levels(x = 'D$LAB_TSC',
            datasources = connections)
  
  # clear the Datashield R sessions and logout
  datashield.logout(connections)

} # }
```
