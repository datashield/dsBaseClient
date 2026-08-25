# Runs the computation if variables are within a table structure

This is an internal function.

## Usage

``` r
meanByClassHelper0b(x, outvar, covar, type, datasources)
```

## Arguments

- x:

  a character, the name of the dataset to get the subsets from.

- outvar:

  a character vector, the names of the continuous variables

- covar:

  a character vector, the names of up to 3 categorical variables

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

a table or a list of tables that hold the length of the numeric
variable(s) and their mean and standard deviation in each subgroup
(subset).

## Details

This function is called by the function 'ds.meanByClass' to produce the
final tables if the user soecify a table structure.

## Author

Gaye, A.
