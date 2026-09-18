# Computes rows and columns sums and means in the server-side

Computes sums and means of rows or columns of a numeric matrix or data
frame on the server-side.

## Usage

``` r
ds.rowColCalc(x = NULL, operation = NULL, newobj = NULL, datasources = NULL)
```

## Arguments

- x:

  a character string specifying the name of a matrix or a data frame.

- operation:

  a character string that indicates the operation to carry out:
  `"rowSums"`, `"colSums"`, `"rowMeans"` or `"colMeans"`.

- newobj:

  a character string that provides the name for the output variable that
  is stored on the data servers. Default `rowcolcalc.newobj`.

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the `datasources` argument is not
  specified the default set of connections will be used: see
  [`datashield.connections_default`](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

## Value

`ds.rowColCalc` returns to the server-side rows and columns sums and
means.

## Details

The function is similar to R base functions `rowSums`, `colSums`,
`rowMeans` and `colMeans` with some restrictions.

The results of the calculation are not returned to the user if they are
potentially revealing i.e. if the number of rows is less than the
allowed number of observations.

Server functions called: `classDS`, `dimDS` and `colnamesDS`

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
  myvar <- list("LAB_TSC","LAB_HDL")
   
  # Log onto the remote Opal training servers
  connections <- DSI::datashield.login(logins = logindata, assign = TRUE, 
  variables = myvar, symbol = "D") 

  
  #Calculate the colSums
  
  ds.rowColCalc(x = "D",
                operation = "colSums", 
                newobj = "D.rowSums", 
                datasources = connections)
                
  #Clear the Datashield R sessions and logout
  datashield.logout(connections) 

} # }
```
