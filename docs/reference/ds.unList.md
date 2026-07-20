# Flattens Server-Side Lists

Coerces an object of list class back to the class it was when it was
coerced into a list.

## Usage

``` r
ds.unList(x.name = NULL, newobj = NULL, datasources = NULL)
```

## Arguments

- x.name:

  a character string specifying the name of the input object to be
  unlisted.

- newobj:

  a character string that provides the name for the output variable that
  is stored on the data servers. Default `unlist.newobj`.

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the `datasources` argument is not
  specified the default set of connections will be used: see
  [`datashield.connections_default`](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

## Value

`ds.unList` returns to the server-side the unlist object.

## Details

This function is similar to the native R function `unlist`.

When an object is coerced to a list, depending on the class of the
original object some information may be lost. Thus, for example, when a
data frame is coerced to list the information that underpins the
structure of the data frame is lost and when it is subject to the
function `ds.unList` it is returned to a simpler class than data frame
e.g. numeric (basically a numeric vector containing all of the original
data in all variables in the data frame but with no structure). If you
wish to reconstruct the original data frame you, therefore, need to
specify this structure again e.g. the column names, etc.

Server function called: `unListDS`

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
  
  #Create a list on the server-side
  
  ds.asList(x.name = "D", 
            newobj = "list.D",
            datasources = connections)
  
  #Flatten a server-side lists
  
  ds.unList(x.name = "list.D",
            newobj = "un.list.D",
           datasources = connections)
 
  # Clear the Datashield R sessions and logout                 
  datashield.logout(connections) 
  
} # }   
```
