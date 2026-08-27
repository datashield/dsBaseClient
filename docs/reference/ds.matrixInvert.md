# Inverts a server-side square matrix

Inverts a square matrix and writes the output to the server-side

## Usage

``` r
ds.matrixInvert(M1 = NULL, newobj = NULL, datasources = NULL)
```

## Arguments

- M1:

  A character string specifying the name of the matrix to be inverted.

- newobj:

  a character string that provides the name for the output variable that
  is stored on the data servers. Default `matrixinvert.newobj`.

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the `datasources` argument is not
  specified the default set of connections will be used: see
  [`datashield.connections_default`](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

## Value

`ds.matrixInvert` returns to the server-side the inverts square matrix.
Also, two validity messages are returned to the client-side indicating
whether the new object has been created in each data source and if so
whether it is in a valid form.

## Details

This operation is only possible if the number of columns and rows of the
matrix are the same and it is non-singular-positive definite (e.g. there
is no row or column that is all zeros).

Server function called: `matrixInvertDS`

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
  
            
  #Example 1: Invert the server-side matrix
  
  #Create the server-side vector 
  
  ds.rUnif(samp.size = 9,
           min = -10.5,
           max = 10.5,
           newobj = "ss.vector.9",
           seed.as.integer = 5575,
           force.output.to.k.decimal.places = 0,
           datasources = connections)
           
  #Create the server-side matrix
           
  ds.matrix(mdata = "ss.vector.9",
            from = "serverside.vector",
            nrows.scalar = 3,
            ncols.scalar = 4,
            byrow = TRUE,
            newobj = "matrix",
            datasources = connections)
   
  #Invert the matrix
  
  ds.matrixInvert(M1 = "matrix",
                  newobj = "matrix.invert",
                  datasources = connections)
  
  # clear the Datashield R sessions and logout
  datashield.logout(connections)
} # }
```
