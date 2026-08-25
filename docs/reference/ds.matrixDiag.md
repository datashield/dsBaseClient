# Calculates matrix diagonals in the server-side

Extracts the diagonal vector from a square matrix or creates a diagonal
matrix based on a vector or a scalar value on the server-side.

## Usage

``` r
ds.matrixDiag(
  x1 = NULL,
  aim = NULL,
  nrows.scalar = NULL,
  newobj = NULL,
  datasources = NULL
)
```

## Arguments

- x1:

  a character string specifying the name of a server-side scalar or
  vector. Also, a numeric value or vector specified from the client-side
  can be speficied. This argument depends on the value specified in
  `aim`. For more information see **Details**.

- aim:

  a character string specifying the behaviour of the function. This can
  be set as: `"serverside.vector.2.matrix"`,
  `"serverside.scalar.2.matrix"`, `"serverside.matrix.2.vector"`,
  `"clientside.vector.2.matrix"` or `"clientside.scalar.2.matrix"`. For
  more information see **Details**.

- nrows.scalar:

  an integer specifying the dimensions of the matrix note that the
  matrix is square (same number of rows and columns). If this argument
  is not specified the matrix dimensions are defined by the length of
  the vector. For more information see **Details**.

- newobj:

  a character string that provides the name for the output variable that
  is stored on the data servers. Default `matrixdiag.newobj`.

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the `datasources` argument is not
  specified the default set of connections will be used: see
  [`datashield.connections_default`](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

## Value

`ds.matrixDiag` returns to the server-side the square matrix diagonal.
Also, two validity messages are returned indicating whether the new
object has been created in each data source and if so whether it is in a
valid form.

## Details

The function behaviour is different depending on the value specified in
the `aim` argument:  
(1) `If aim = "serverside.vector.2.matrix"` the function takes a
server-side vector and writes out a square matrix with the vector as its
diagonal and all off-diagonal `values = 0`. The dimensions of the output
matrix are determined by the length of the vector. If the vector length
is `k`, the output matrix has `k` rows and `k` columns.  
(2) If `aim = "serverside.scalar.2.matrix"` the function takes a
server-side scalar and writes out a square matrix with all diagonal
values equal to the value of the scalar and all off-diagonal
`values = 0`. The dimensions of the square matrix are determined by the
value of the `nrows.scalar` argument.  
(3) If `aim = "serverside.matrix.2.vector"` the function takes a square
server-side matrix and extracts its diagonal values as a vector which is
written to the server-side.  
(4) If `aim = "clientside.vector.2.matrix"` the function takes a vector
specified on the client-side and writes out a square matrix to the
server-side with the vector as its diagonal and all off-diagonal
`values = 0`. The dimensions of the output matrix are determined by the
length of the vector.  
(5) If `aim = "clientside.scalar.2.matrix"` the function takes a scalar
specified on the client-side and writes out a square matrix with all
diagonal values equal to the value of the scalar. The dimensions of the
square matrix are determined by the value of the `nrows.scalar`
argument.

If `x1` is a vector and the `nrows.scalar` is set as `k`, the vector
will be used repeatedly to fill up the diagonal. For example, the vector
is of length 7 and `nrows.scalar = 18`, a square diagonal matrix with 18
rows and 18 columns will be created.

Server function called: `matrixDiagDS`

## Author

DataSHIELD Development Team

## Examples
