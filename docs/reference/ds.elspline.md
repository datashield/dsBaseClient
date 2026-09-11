# Basis for a piecewise linear spline with meaningful coefficients

This function is based on the native R function `elspline` from the
`lspline` package. This function computes the basis of piecewise-linear
spline such that, depending on the argument marginal, the coefficients
can be interpreted as (1) slopes of consecutive spline segments, or (2)
slope change at consecutive knots.

## Usage

``` r
ds.elspline(
  x,
  n,
  marginal = FALSE,
  names = NULL,
  newobj = NULL,
  datasources = NULL
)
```

## Arguments

- x:

  the name of the input numeric variable

- n:

  integer greater than 2, knots are computed such that they cut n
  equally-spaced intervals along the range of x

- marginal:

  logical, how to parametrise the spline, see Details

- names:

  character, vector of names for constructed variables

- newobj:

  a character string that provides the name for the output variable that
  is stored on the data servers. Default `elspline.newobj`.

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the `datasources` argument is not
  specified the default set of connections will be used: see
  [`datashield.connections_default`](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

## Value

an object of class "lspline" and "matrix", which its name is specified
by the `newobj` argument (or its default name "elspline.newobj"), is
assigned on the serverside.

## Details

If marginal is FALSE (default) the coefficients of the spline correspond
to slopes of the consecutive segments. If it is TRUE the first
coefficient correspond to the slope of the first segment. The
consecutive coefficients correspond to the change in slope as compared
to the previous segment. Function elspline wraps lspline and computes
the knot positions such that they cut the range of x into n equal-width
intervals.

## Author

Demetris Avraam for DataSHIELD Development Team
