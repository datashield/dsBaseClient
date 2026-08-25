# Recodes the levels of a server-side factor vector

The function replaces the levels of a factor by the specified new ones.

## Usage

``` r
ds.recodeLevels(
  x = NULL,
  newCategories = NULL,
  newobj = NULL,
  datasources = NULL
)
```

## Arguments

- x:

  a character string specifying the name of a factor variable.

- newCategories:

  a character vector specifying the new levels. Its length must be equal
  or greater to the current number of levels.

- newobj:

  a character string that provides the name for the output object that
  is stored on the data servers. Default `recodelevels.newobj`.

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the `datasources` argument is not
  specified the default set of connections will be used: see
  [`datashield.connections_default`](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

## Value

`ds.recodeLevels` returns to the server-side a variable of type factor
with the replaces levels.

## Details

This function is similar to native R function
[`levels()`](https://rdrr.io/r/base/levels.html).

It can for example be used to merge two classes into one, to add a
level(s) to a vector or to rename (i.e. re-label) the levels of a
vector.

Server function called: [`levels()`](https://rdrr.io/r/base/levels.html)

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
  
  # Recode the levels of a factor variable
  
  ds.recodeLevels(x = "D$PM_BMI_CATEGORICAL",
                  newCategories = c("1","2","3"),
                  newobj = "BMI_CAT",
                  datasources = connections)
                 
  # Clear the Datashield R sessions and logout                 
  datashield.logout(connections) 
  
} # }   
```
