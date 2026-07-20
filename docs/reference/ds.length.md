# Gets the length of an object in the server-side

This function gets the length of a vector or list that is stored on the
server-side. This function is similar to the R function `length`.

## Usage

``` r
ds.length(
  x = NULL,
  type = "both",
  datasources = NULL,
  classConsistencyCheck = TRUE
)
```

## Arguments

- x:

  a character string specifying the name of a vector or list.

- type:

  a character that represents the type of analysis to carry out. If
  `type` is set to `'combine'`, `'combined'`, `'combines'` or `'c'`, a
  global length is returned if `type` is set to `'split'`, `'splits'` or
  `'s'`, the length is returned separately for each study. if `type` is
  set to `'both'` or `'b'`, both sets of outputs are produced. Default
  `'both'`.

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

`ds.length` returns to the client-side the pooled length of a vector or
a list, or the length of a vector or a list for each study separately.

## Details

Server function called: `lengthDS`

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
  
  # Example 1: Get the total number of observations of the vector of
  # variable 'LAB_TSC' across all the studies
  ds.length(x = 'D$LAB_TSC', 
            type = 'combine',
            datasources = connections)

  # Example 2: Get the number of observations of the vector of variable
  # 'LAB_TSC' for each study separately
  ds.length(x = 'D$LAB_TSC',
            type = 'split',
            datasources = connections)

  # Example 3: Get the number of observations on each study and the total
  # number of observations across all the studies for the variable 'LAB_TSC'
  ds.length(x = 'D$LAB_TSC',
            type = 'both',
            datasources = connections)
  
  # clear the Datashield R sessions and logout
  datashield.logout(connections)
} # }
```
