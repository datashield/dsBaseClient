# Computes the mean values of a numeric vector across a factor vector

This is an internal function.

## Usage

``` r
meanByClassHelper0a(a, b, type, datasources)
```

## Arguments

- a:

  a character, the name of a numeric vector.

- b:

  a character, the name of a factor vector.

- type:

  a character which represents the type of analysis to carry out. If
  `type` is set to 'combine', a pooled table of results is generated. If
  `type` is set to 'split', a table of results is genrated for each
  study.

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the \<datasources\> the default set
  of connections will be used: see
  [datashield.connections_default](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

## Value

a table or a list of tables that hold the length of the numeric variable
and its mean and standard deviation in each subgroup (subset).

## Details

This function is called by the function 'ds.meanByClass' to produce the
final tables if the user specifies two loose vectors.

## Author

Gaye, A.
