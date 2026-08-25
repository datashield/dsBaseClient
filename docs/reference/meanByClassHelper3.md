# Generates results tables for each study separately

This is an internal function.

## Usage

``` r
meanByClassHelper3(dtsources, tablenames, variables, invalidrecorder)
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

  a list, holds informations about invalid subsets in each study

## Value

a list which one results table for each study.

## Details

This function is called by the function 'ds.meanByClass' to produce the
final tables if the user sets the parmater 'type' to 'split'.

## Author

Gaye, A.
