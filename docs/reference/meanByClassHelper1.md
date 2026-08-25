# Generates subset tables

This is an internal function.

## Usage

``` r
meanByClassHelper1(dtsource, tables, variable, categories)
```

## Arguments

- dtsource:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the \<datasources\> the default set
  of connections will be used: see
  [datashield.connections_default](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

- tables:

  a character vector, the tables to breakdown

- variable:

  a character, the variable to subset on

- categories:

  a character vector, the classes in the variables to subset on

## Value

a character the names of the new subset tables.

## Details

This function is called by the function 'ds.meanByClass' to break down
the initial table by the specified categorical variables.

## Author

Gaye, A.
