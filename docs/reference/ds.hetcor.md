# Heterogeneous Correlation Matrix

This function is based on the hetcor function from the R package
`polycor`.

## Usage

``` r
ds.hetcor(
  data = NULL,
  ML = TRUE,
  std.err = TRUE,
  bins = 4,
  pd = TRUE,
  use = "complete.obs",
  datasources = NULL
)
```

## Arguments

- data:

  the name of a data frame consisting of factors, ordered factors,
  logical variables, character variables, and/or numeric variables, or
  the first of several variables.

- ML:

  if TRUE, compute maximum-likelihood estimates; if FALSE (default),
  compute quick two-step estimates.

- std.err:

  if TRUE (default), compute standard errors.

- bins:

  number of bins to use for continuous variables in testing bivariate
  normality; the default is 4.

- pd:

  if TRUE (default) and if the correlation matrix is not
  positive-definite, an attempt will be made to adjust it to a
  positive-definite matrix, using the nearPD function in the Matrix
  package. Note that default arguments to nearPD are used (except
  corr=TRUE); for more control call nearPD directly.

- use:

  if "complete.obs", remove observations with any missing data; if
  "pairwise.complete.obs", compute each correlation using all
  observations with valid data for that pair of variables.

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the `datasources` argument is not
  specified the default set of connections will be used: see
  [`datashield.connections_default`](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

## Value

Returns an object of class "hetcor" from each study, with the following
components: the correlation matrix; the type of each correlation:
"Pearson", "Polychoric", or "Polyserial"; the standard errors of the
correlations, if requested; the number (or numbers) of observations on
which the correlations are based; p-values for tests of bivariate
normality for each pair of variables; the method by which any missing
data were handled: "complete.obs" or "pairwise.complete.obs"; TRUE for
ML estimates, FALSE for two-step estimates.

## Details

Computes a heterogenous correlation matrix, consisting of Pearson
product-moment correlations between numeric variables, polyserial
correlations between numeric and ordinal variables, and polychoric
correlations between ordinal variables.

## Author

Demetris Avraam for DataSHIELD Development Team
