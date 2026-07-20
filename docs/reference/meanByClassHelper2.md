# Generates a table for pooled results

This is an internal function.

## Usage

``` r
meanByClassHelper2(dtsources, tablenames, variables, invalidrecorder)
```

## Arguments

- dtsources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the \<datasources\> the default set
  of connections will be used: see
  [datashield.connections_default](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

- tablenames:

  a character vector, the name of the subset tables

- variables:

  a character vector, the names of the continuous variables to computes
  a mean for.

- invalidrecorder:

  a list, holds information about invalid subsets in each study.

## Value

a matrix, a table which contains the length, mean and standard deviation
of each of the specified 'variables' in each subset table.

## Details

This function is called by the function 'ds.meanByClass' to produce the
final table if the user sets the parameter 'type' to combine (the
default behaviour of 'ds.meanByClass').

## Author

Gaye, A.

Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
