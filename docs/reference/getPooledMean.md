# Gets a pooled statistical mean

This is an internal function.

## Usage

``` r
getPooledMean(dtsources, x)
```

## Arguments

- dtsources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the \<datasources\> the default set
  of connections will be used: see
  [datashield.connections_default](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

- x:

  a character, the name of a numeric vector

## Value

a pooled mean

## Details

This function is called to avoid calling the client function 'ds.mean'
which may stop the process due to some checks not required when
computing a mean inside a function.
