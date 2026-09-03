# Checks an object has been generated on the server side

This is an internal function.

## Usage

``` r
isAssigned(datasources = NULL, newobj = NULL)
```

## Arguments

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the \<datasources\> the default set
  of connections will be used: see
  [datashield.connections_default](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

- newobj:

  a character, the name the object to look for.

## Value

nothing is return but the process is stopped if the object was not
generated in any one server.

## Details

After calling an assign function it is important to know whether or not
the action has been completed by checking if the output actually exists
on the server side.
