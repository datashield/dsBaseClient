# Identifies complete cases in server-side R objects

Selects complete cases of a data frame, matrix or vector that contain
missing values.

## Usage

``` r
ds.completeCases(x1 = NULL, newobj = NULL, datasources = NULL)
```

## Arguments

- x1:

  a character denoting the name of the input object which can be a data
  frame, matrix or vector.

- newobj:

  a character string that provides the name for the complete-cases
  object that is stored on the data servers. If the user does not
  specify a name, then the function generates a name for the generated
  object that is the name of the input object with the suffix
  "\_complete.cases"

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the `datasources` argument is not
  specified, the default set of connections will be used: see
  [`datashield.connections_default`](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

## Value

`ds.completeCases` generates a modified data frame, matrix or vector
from which all rows containing at least one NA have been deleted. The
output object is stored on the server-side. Only two validity messages
are returned to the client-side indicating the name of the `newobj` that
has been created in each data source and if it is in a valid form.

## Details

In the case of a data frame or matrix, `ds.completeCases` deletes all
rows containing one or more missing values. However `ds.completeCases`
in vectors only deletes the observation recorded as NA.

Server function called: `completeCasesDS`

## Author

DataSHIELD Development Team

## Examples

``` r
if (FALSE) { # \dontrun{
  ## Version 6, for version 5 see the Wiki
  # Connecting to the Opal servers

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
  
  # Log onto the remote Opal training servers
  connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D") 

  # Select complete cases from different R objects

  ds.completeCases(x1 = "D", #data frames in the Opal servers 
                             #(see above the connection to the Opal servers)
                   newobj = "D.completeCases", # name for the output object 
                                               # that is stored in the Opal servers
                   datasources = connections)  # All Opal servers are used 
                                               # (see above the connection to the Opal servers)
                 
  ds.completeCases(x1 = "D$LAB_TSC", #vector (variable) of the data frames in the Opal servers 
                                     #(see above the connection to the Opal servers)
                   newobj = "LAB_TSC.completeCases", #name for the output variable 
                                                     #that is stored in the Opal servers
                   datasources = connections[2]) #only the second Opal server is used ("study2")
                   
  # Clear the Datashield R sessions and logout
  datashield.logout(connections) 
  } # }
  
```
