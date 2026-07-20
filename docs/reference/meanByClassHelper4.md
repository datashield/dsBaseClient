# Gets the subset tables out of the list (i.e. unlist)

This is an internal function.

## Usage

``` r
meanByClassHelper4(
  dtsource,
  alist,
  initialtable,
  variable = NA,
  categories = NA
)
```

## Arguments

- dtsource:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the \<datasources\> the default set
  of connections will be used: see
  [datashield.connections_default](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

- alist:

  the name of the list that holds the final subset tables

- initialtable:

  a character the name of the table that the subset were generated from

- variable:

  a character, the variable to subset on

- categories:

  a character vector, the classes in the variables to subset on

## Value

the 'loose' subset tables are stored on the server side

## Details

This function is called by the function 'ds.meanByClass' to obtain
'loose' subset tables because the 'subsetByClass' function does not
handle a table within a list.

## Author

Gaye, A.
