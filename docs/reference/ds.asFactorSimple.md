# Converts a numeric vector into a factor

ds.asFactorSimple calls the assign function asFactorSimpleDS and thereby
coerces a numeric or character vector into a factor

## Usage

``` r
ds.asFactorSimple(
  input.var.name = NULL,
  newobj.name = NULL,
  datasources = NULL
)
```

## Arguments

- input.var.name:

  a character string which provides the name of the variable to be
  converted to a factor.

- newobj.name:

  a character string that provides the name for the output variable that
  is stored on the data servers. Default `asfactor.newobj`.

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the `datasources` argument is not
  specified the default set of connections will be used: see
  [`datashield.connections_default`](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

## Value

an output vector of class factor to the serverside. In addition, returns
a validity message with the name of the created object on the
client-side and if creation fails an error message which can be viewed
using datashield.errors().

## Details

The function converts the input variable into a factor. Unlike
ds.asFactor and its serverside functions, ds.asFactorSimple does no more
than coerce the class of a variable to make it a factor on the
serverside in each data source. It does not check for or enforce
consistency of factor levels across sources or allow you to force an
arbitrary set of levels unless those levels actually exist in the
sources. Furthermore, it does not allow you to create an array of binary
dummy variables that is equivalent to a factor. If you need to do any of
these things you will have to use the ds.asFactor function.

## Author

DataSHIELD Development Team
