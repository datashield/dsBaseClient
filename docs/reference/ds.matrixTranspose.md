# Transposes a server-side matrix

Transposes a matrix and writes the output to the server-side

## Usage

``` r
ds.matrixTranspose(M1 = NULL, newobj = NULL, datasources = NULL)
```

## Arguments

- M1:

  a character string specifying the name of the matrix.

- newobj:

  a character string that provides the name for the output variable that
  is stored on the data servers. Default `matrixtranspose.newobj`.

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the `datasources` argument is not
  specified the default set of connections will be used: see
  [`datashield.connections_default`](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

## Value

`ds.matrixTranspose` returns to the server-side the transpose matrix.
Also, two validity messages are returned to the client-side indicating
whether the new object has been created in each data source and if so
whether it is in a valid form.

## Details

This operation converts matrix `A` to matrix `C` where element `C[i,j]`
of matrix `C` equals element `A[j,i]` of matrix `A`. Matrix `A`,
therefore, has the same number of rows as matrix `C` has columns and
vice versa.

Server function called: `matrixTransposeDS`

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
  
            
  #Example 1: Transpose the server-side matrix
  
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
   
  #Transpose the matrix
  
  ds.matrixTranspose(M1 = "matrix",
                     newobj = "matrix.transpose",
                     datasources = connections)
  
  # clear the Datashield R sessions and logout
  datashield.logout(connections)
} # }
```
