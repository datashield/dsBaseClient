# Generates a density grid in the client-side

This function generates a grid density object which can then be used to
produced heatmap or contour plots.

## Usage

``` r
ds.densityGrid(
  x = NULL,
  y = NULL,
  numints = 20,
  type = "combine",
  datasources = NULL
)
```

## Arguments

- x:

  a character string providing the name of the input numerical vector.

- y:

  a character string providing the name of the input numerical vector.

- numints:

  an integer, the number of intervals for the grid density object. The
  default value is 20.

- type:

  a character string that represents the type of graph to display. If
  `type` is set to `'combine'`, a pooled grid density matrix is
  generated, instead if `type` is set to `'split'` one grid density
  matrix is generated. Default `'combine'`.

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the `datasources` argument is not
  specified the default set of connections will be used: see
  [`datashield.connections_default`](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

## Value

`ds.densityGrid` returns a grid density matrix.

## Details

The cells with a count \> 0 and \< nfilter.tab are considered invalid
and the count is set to 0.

In DataSHIELD the user does not have access to the micro-data so and
extreme values such as the maximum and the minimum are potentially
non-disclosive so this function does not allow for the user to set the
limits of the density grid and the minimum and maximum values of the `x`
and `y` vectors. These elements are set by the server-side function
`densityGridDS` to 'valid' values (i.e. values that do not lead to
leakage of micro-data to the user).

Server function called: `densityGridDS`

## Author

DataSHIELD Development Team

## Examples
