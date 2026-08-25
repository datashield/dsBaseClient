# Creates a matrix on the server-side

Creates a matrix on the server-side with dimensions specified by
`nrows.scalar` and `ncols.scalar` arguments and assigns the values of
all its elements based on the `mdata` argument.

## Usage

``` r
ds.matrix(
  mdata = NA,
  from = "clientside.scalar",
  nrows.scalar = NULL,
  ncols.scalar = NULL,
  byrow = FALSE,
  dimnames = NULL,
  newobj = NULL,
  datasources = NULL
)
```

## Arguments

- mdata:

  a character string specifying the name of a server-side scalar or
  vector. Also, a numeric value representing a scalar specified from the
  client-side can be speficied. Zeros, negative values and NAs are all
  allowed. For more information see **Details**.

- from:

  a character string specifying the source and nature of `mdata`. This
  can be set as `"serverside.vector"`, `"serverside.scalar"` or
  `"clientside.scalar"`. Default `"clientside.scalar"`.

- nrows.scalar:

  an integer or a character string that specifies the number of rows in
  the matrix to be created. For more information see **Details**.

- ncols.scalar:

  an integer or a character string that specifies the number of columns
  in the matrix to be created.

- byrow:

  logical. If TRUE and `mdata` is a vector the matrix created should be
  filled row by row. If FALSE the matrix created should be filled column
  by column. Default = FALSE.

- dimnames:

  a list of length 2 giving the row and column names respectively.

- newobj:

  a character string that provides the name for the output variable that
  is stored on the data servers. Default `matrix.newobj`.

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the `datasources` argument is not
  specified the default set of connections will be used: see
  [`datashield.connections_default`](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

## Value

`ds.matrix` returns the created matrix which is written on the
server-side. In addition, two validity messages are returned indicating
whether the new matrix has been created in each data source and if so
whether it is in a valid form.

## Details

This function is similar to the R native function
[`matrix()`](https://rdrr.io/r/base/matrix.html).

If in the `mdata` argument a vector is specified this should have the
same length as the total number of elements in the matrix. If this is
not TRUE the values in `mdata` will be used repeatedly until all
elements in the matrix are full. If `mdata` argument is a scalar, all
elements in the matrix will take that value.

In the `nrows.scalar` argument can be a character string specifying the
name of a server-side scalar. For example, if a server-side scalar named
`ss.scalar` exists and holds the value 23, then by specifying
`nrows.scalar = "ss.scalar"`, the matrix created will have 23 rows. Also
this argument can be a numeric value from the client-side. The same
rules are applied to `ncols.scalar` argument but in this case the column
numbers are specified. In both arguments a zero, negative, NULL or
missing value is not permitted.

Server function called: `matrixDS`

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
  
  #Example 1: create a matrix with -13 value in all elements
  
  ds.matrix(mdata = -13,
            from = "clientside.scalar",
            nrows.scalar = 3,
            ncols.scalar = 8,
            newobj = "cs.block",
            datasources = connections)
            
  #Example 2: create a matrix of missing values 

  ds.matrix(NA,
            from = "clientside.scalar",
            nrows.scalar = 4,
            ncols.scalar = 5,
            newobj = "cs.block.NA",
            datasources = connections)

  #Example 3: create a matrix using a server-side vector
  #create a vector in the server-side
  
  ds.rUnif(samp.size = 45,
           min = -10.5,
           max = 10.5,
           newobj = "ss.vector",
           seed.as.integer = 8321,
           force.output.to.k.decimal.places = 0,
           datasources = connections)
           
  ds.matrix(mdata = "ss.vector",
            from = "serverside.vector",
            nrows.scalar = 5,
            ncols.scalar = 9,
            newobj = "sv.block",
            datasources = connections)
            
  #Example 4: create a matrix using a server-side vector and specifying
  #the row a column names

  ds.rUnif(samp.size = 9,
           min = -10.5,
           max = 10.5,
           newobj = "ss.vector.9",
           seed.as.integer = 5575,
           force.output.to.k.decimal.places = 0,
           datasources = connections)
           
  ds.matrix(mdata = "ss.vector.9",
            from = "serverside.vector",
            nrows.scalar = 5,
            ncols.scalar = 9,
            byrow = TRUE,
            dimnames = list(c("a","b","c","d","e")),
            newobj = "sv.block.9.dimnames1",
            datasources = connections)

  
  # clear the Datashield R sessions and logout
  datashield.logout(connections)
} # }
```
