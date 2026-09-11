# Ensures that the requested subset is not larger than the original object

Compares subset and original object sizes and eventually carries out
subsetting.

## Usage

``` r
subsetHelper(dts, data, rs = NULL, cs = NULL)
```

## Arguments

- dts:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the `datasources` argument is not
  specified the default set of connections will be used: see
  [`datashield.connections_default`](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

- data:

  a character string specifying the name of the data frame or the factor
  vector and the range of the subset.

- rs:

  a vector of two integers specifying the indices of the rows de
  extract.

- cs:

  a vector of two integers or one or more characters.

## Value

`subsetHelper` returns a message or the class of the object if the
object has the same class in all studies.

## Details

This function is called by the function `ds.subset` to ensure that the
requested subset is not larger than the original object.

This function is internal.

Server function called: `dimDS`

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
  
  subsetHelper(dts = connections, 
               data = "D", 
               rs = 1:10, 
               cs = c("D$LAB_TSC","D$LAB_TRIG"))  
                      
  # clear the Datashield R sessions and logout
  datashield.logout(connections)
} # }
```
