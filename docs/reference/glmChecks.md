# Checks if the elements in the glm model have the right characteristics

This is an internal function required by the client function `ds.glm` to
verify all the variables and ensure the process does not halt
inadvertently

## Usage

``` r
glmChecks(formula, data, offset, weights, datasources)
```

## Arguments

- formula:

  a character, a regression formula given as a string character

- data:

  a character, the name of an optional data frame containing the
  variables in in the `formula`.

- offset:

  null or a numeric vector that can be used to specify an a priori known
  component to be included in the linear predictor during fitting.

- weights:

  a character, the name of an optional vector of 'prior weights' to be
  used in the fitting process. Should be NULL or a numeric vector.

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the \<datasources\> the default set
  of connections will be used: see
  [datashield.connections_default](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

## Value

an integer 0 if check was passed and 1 if failed

## Details

the variables are checked to ensure they are defined, not empty (i.e.
are not missing at complete) and eventually (if 'offset' or 'weights')
are of 'numeric' with non negative value (if 'weights').

## Author

Gaye, A.

Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
