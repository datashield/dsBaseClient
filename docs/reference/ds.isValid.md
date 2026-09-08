# Checks if a server-side object is valid

Checks if a vector or table structure has a number of observations equal
to or greater than the threshold set by DataSHIELD.

## Usage

``` r
ds.isValid(x = NULL, datasources = NULL)
```

## Arguments

- x:

  a character string specifying the name of a vector, dataframe or
  matrix.

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the `datasources` argument is not
  specified the default set of connections will be used: see
  [`datashield.connections_default`](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

## Value

`ds.isValid` returns a boolean. If it is TRUE input object is valid,
FALSE otherwise.

## Details

In DataSHIELD, analyses are possible only on valid objects to ensure the
output is not disclosive. This function checks if an input object is
valid. A vector is valid if the number of observations is equal to or
greater than a set threshold. A factor vector is valid if all its levels
(categories) have a count equal or greater than the set threshold. A
data frame or a matrix is valid if the number of rows is equal or
greater than the set threshold.

Server function called: `isValidDS`

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
  
  # Check if the dataframe assigned above is valid
  ds.isValid(x = 'D',
             datasources = connections) #all servers are used
  ds.isValid(x = 'D',
             datasources = connections[2]) #only the second server is used (study2)
 
  # clear the Datashield R sessions and logout
  datashield.logout(connections)

} # }
```
