# Generate a Basis Matrix for Natural Cubic Splines

This function is based on the native R function `ns` from the `splines`
package. This function generate the B-spline basis matrix for a natural
cubic spline.

## Usage

``` r
ds.ns(
  x,
  df = NULL,
  knots = NULL,
  intercept = FALSE,
  Boundary.knots = NULL,
  newobj = NULL,
  datasources = NULL
)
```

## Arguments

- x:

  the predictor variable. Missing values are allowed.

- df:

  degrees of freedom. One can supply df rather than knots; ns() then
  chooses df - 1 - intercept knots at suitably chosen quantiles of x
  (which will ignore missing values). The default, df = NULL, sets the
  number of inner knots as length(knots).

- knots:

  breakpoints that define the spline. The default is no knots; together
  with the natural boundary conditions this results in a basis for
  linear regression on x. Typical values are the mean or median for one
  knot, quantiles for more knots. See also Boundary.knots.

- intercept:

  if TRUE, an intercept is included in the basis; default is FALSE.

- Boundary.knots:

  boundary points at which to impose the natural boundary conditions and
  anchor the B-spline basis (default the range of the data). If both
  knots and Boundary.knots are supplied, the basis parameters do not
  depend on x. Data can extend beyond Boundary.knots.

- newobj:

  a character string that provides the name for the output variable that
  is stored on the data servers. Default `ns.newobj`.

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the `datasources` argument is not
  specified the default set of connections will be used: see
  [`datashield.connections_default`](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

## Value

A matrix of dimension length(x) \* df where either df was supplied or if
knots were supplied, df = length(knots) + 1 + intercept. Attributes are
returned that correspond to the arguments to ns, and explicitly give the
knots, Boundary.knots etc for use by predict.ns(). The object is
assigned at each serverside.

## Details

`ns` is native R is based on the function `splineDesign`. It generates a
basis matrix for representing the family of piecewise-cubic splines with
the specified sequence of interior knots, and the natural boundary
conditions. These enforce the constraint that the function is linear
beyond the boundary knots, which can either be supplied or default to
the extremes of the data. A primary use is in modeling formula to
directly specify a natural spline term in a model.

## Author

Demetris Avraam for DataSHIELD Development Team
