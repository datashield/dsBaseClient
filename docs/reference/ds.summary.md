# Generates the summary of a server-side object

Generates the summary of a server-side object.

## Usage

``` r
ds.summary(x = NULL, datasources = NULL)
```

## Arguments

- x:

  a character string specifying the name of a numeric or factor
  variable.

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the `datasources` argument is not
  specified the default set of connections will be used: see
  [`datashield.connections_default`](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

## Value

`ds.summary` returns to the client-side the class and size of the
server-side object. Also other information is returned depending on the
class of the object. For example, potentially disclosive information
such as the minimum and maximum values of numeric vectors are not
returned. The summary is given for each study separately.

## Details

This function provides some insight about an object. Unlike the similar
native R `summary` function only a limited class of objects can be used
as input to reduce the risk of disclosure. For example, the minimum and
the maximum values of a numeric vector are not given to the client
because they are potentially disclosive.

server functions called: `isValidDS`, `dimDS` and `colnamesDS`

## Author

DataSHIELD Development Team

Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands

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
  
  #Calculate the summary of a numeric variable
  
  ds.summary(x = "D$LAB_TSC",
             datasources = connections)
 
  #Calculate the summary of a factor variable

  ds.summary(x = "D$PM_BMI_CATEGORICAL",
             datasources = connections)
                                
  # Clear the Datashield R sessions and logout  
  datashield.logout(connections) 

} # }
```
