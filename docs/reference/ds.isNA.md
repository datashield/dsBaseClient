# Checks if a server-side vector is empty

this function is similar to R function `is.na` but instead of a vector
of booleans it returns just one boolean to tell if all the elements are
missing values.

## Usage

``` r
ds.isNA(x = NULL, datasources = NULL, classConsistencyCheck = TRUE)
```

## Arguments

- x:

  a character string specifying the name of the vector to check.

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the `datasources` argument is not
  specified the default set of connections will be used: see
  [`datashield.connections_default`](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

- classConsistencyCheck:

  logical. If TRUE, checks that the input object has the same class
  across all studies. Default TRUE.

## Value

`ds.isNA` returns a boolean. If it is TRUE the vector is empty (all
values are NA), FALSE otherwise.

## Details

In certain analyses such as GLM none of the variables should be missing
at complete (i.e. missing value for each observation). Since in
DataSHIELD it is not possible to see the data it is important to know
whether or not a vector is empty to proceed accordingly.

Server function called: `isNaDS`

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

  # check if all the observation of the variable 'LAB_HDL' are missing (NA)
  ds.isNA(x = 'D$LAB_HDL',
          datasources = connections) #all servers are used
  ds.isNA(x = 'D$LAB_HDL',
          datasources = connections[1]) #only the first server is used (study1)


  # clear the Datashield R sessions and logout
  datashield.logout(connections)

} # }
```
