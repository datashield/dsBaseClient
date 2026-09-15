# Performs a mathematical operation on two or more vectors

Carries out a row-wise operation on two or more vector. The function
calls no server side function; it uses the R operation symbols built in
DataSHIELD.

## Usage

``` r
ds.vectorCalc(x = NULL, calc = NULL, newobj = NULL, datasources = NULL)
```

## Arguments

- x:

  a vector of characters, the names of the vectors to include in the
  operation.

- calc:

  a character, a symbol that indicates the mathematical operation to
  carry out: '+' for addition, '/' for division, \*' for multiplication
  and '-' for subtraction.

- newobj:

  the name of the output object. By default the name is
  'vectorcalc.newobj'.

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the \<datasources\> the default set
  of connections will be used: see
  [datashield.connections_default](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

## Value

no data are returned to user, the output vector is stored on the server
side.

## Details

In DataSHIELD it is possible to perform an operation on vectors by just
using the relevant R symbols (e.g. '+' for addition, '\*' for
multiplication, '-' for subtraction and '/' for division). This might
however be inconvenient if the number of vectors to include in the
operation is large. This function takes the names of two or more vectors
and performs the desired operation which could be an addition, a
multiplication, a subtraction or a division. If one or more vectors have
a missing value at any one entry (i.e. observation), the operation
returns a missing value ('NA') for that entry; the output vectors has,
hence the same length as the input vectors.

## Author

Gaye, A.

## Examples

``` r
if (FALSE) { # \dontrun{

  # load the file that contains the login details
  data(logindata)

  # login and assign the required variables to R
  myvar <- list('LAB_TSC','LAB_HDL')
  conns <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)

  # performs an addtion of 'LAB_TSC' and 'LAB_HDL'
  myvectors <- c('D$LAB_TSC', 'D$LAB_HDL')
  ds.vectorCalc(x=myvectors, calc='+')

  # clear the Datashield R sessions and logout
  datashield.logout(conns)

} # }
```
