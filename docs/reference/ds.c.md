# Combines values into a vector or list in the server-side

Concatenates objects into one vector.

## Usage

``` r
ds.c(x = NULL, newobj = NULL, datasources = NULL, classConsistencyCheck = TRUE)
```

## Arguments

- x:

  a vector of character string providing the names of the objects to be
  combined.

- newobj:

  a character string that provides the name for the output object that
  is stored on the data servers. Default `c.newobj`.

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the `datasources` argument is not
  specified the default set of connections will be used: see
  [`datashield.connections_default`](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

- classConsistencyCheck:

  logical. If TRUE, verifies that each input object has the same class
  across all studies before concatenation. Default TRUE.

## Value

`ds.c` returns the vector of concatenating R objects which are written
to the server-side.

## Details

To avoid combining the character names and not the vectors on the
client-side, the names are coerced into a list and the server-side
function loops through that list to concatenate the list's elements into
a vector.

Server function called: `cDS`

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
  
  # Create a vector with combined objects
  myvect <- c("D$LAB_TSC", "D$LAB_HDL")
  ds.c(x = myvect,
       newobj = "new.vect",
       datasources = connections[1]) #only the first Opal server is used ("study1")
                
  # Clear the Datashield R sessions and logout                 
  datashield.logout(connections) 
  
} # }    
```
