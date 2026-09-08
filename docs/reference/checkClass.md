# Checks that an object has the same class in all studies

This is an internal function.

## Usage

``` r
checkClass(datasources = NULL, obj = NULL)
```

## Arguments

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the \<datasources\> the default set
  of connections will be used: see
  [datashield.connections_default](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

- obj:

  a string character, the name of the object to check for.

## Value

a message or the class of the object if the object has the same class in
all studies.

## Details

In DataSHIELD an object included in analysis must be of the same type in
all the collaborating studies. If that is not the case the process is
stopped
