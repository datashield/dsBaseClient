# Generates a Heat Map plot

Generates a heat map plot of the pooled data or one plot for each
dataset.

## Usage

``` r
ds.heatmapPlot(
  x = NULL,
  y = NULL,
  type = "combine",
  show = "all",
  numints = 20,
  method = "smallCellsRule",
  k = 3,
  noise = 0.25,
  datasources = NULL
)
```

## Arguments

- x:

  a character string specifying the name of a numerical vector.

- y:

  a character string specifying the name of a numerical vector.

- type:

  a character string that represents the type of graph to display.
  `type` argument can be set as `'combine'` or `'split'`. Default
  `'combine'`. For more information see **Details**.

- show:

  a character string that represents where the plot should be focused.
  `show` argument can be set as `'all'` or `'zoomed'`. Default `'all'`.
  For more information see **Details**.

- numints:

  the number of intervals for a density grid object. Default `numints`
  value is `20`.

- method:

  a character string that defines which heat map will be created. The
  `method` argument can be set as `'smallCellsRule'`, `'deterministic'`
  or `'probabilistic'`. Default `'smallCellsRule'`. For more information
  see **Details**.

- k:

  the number of the nearest neighbours for which their centroid is
  calculated. Default `k` value is `3`. For more information see
  **Details**.

- noise:

  the percentage of the initial variance that is used as the variance of
  the embedded noise if the argument `method` is set to
  `'probabilistic'`. Default `noise` value is `0.25`. For more
  information see **Details**.

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the `datasources` argument is not
  specified the default set of connections will be used: see
  [`datashield.connections_default`](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

## Value

`ds.heatmapPlot` returns to the client-side a heat map plot and a
message specifying the number of invalid cells in each study.

## Details

The `ds.heatmapPlot` function first generates a density grid and uses it
to plot the graph. Cells of the grid density matrix that hold a count of
less than the filter set by DataSHIELD (usually 5) are considered
invalid and turned into 0 to avoid potential disclosure. A message is
printed to inform the user about the number of invalid cells. The ranges
returned by each study and used in the process of getting the grid
density matrix are not the exact minimum and maximum values but rather
close approximates of the real minimum and maximum value. This was done
to reduce the risk of potential disclosure.

In the argument `type` can be specified two types of graphics to
display:

- `'combine'`:

  : a combined heat map plot is displayed

- `'split'`:

  : each heat map is plotted separately

In the argument `show` can be specified two options:

- `'all'`:

  : the ranges of the variables are used as plot limits

- `'zoomed'`:

  : the plot is zoomed to the region where the actual data are

In the argument `method` can be specified 3 different heat map to be
created:

- `'smallCellsRule'`:

  : the heat map of the actual variables is created but grids with low
  counts are replaced with grids with zero counts

- `'deterministic'`:

  : the heat map of the scaled centroids of each `k` nearest neighbours
  of the original variables are created, where the value of `k` is set
  by the user

- `'probabilistic'`:

  : the heat map of `'noisy'` variables is generated. The added noise
  follows a normal distribution with zero mean and variance equal to a
  percentage of the initial variance of each input variable. This
  percentage is specified by the user in the argument `noise`

In the `k` argument the user can choose any value for `k` equal to or
greater than the pre-specified threshold used as a disclosure control
for this method and lower than the number of observations minus the
value of this threshold. By default the value of `k` is set to be equal
to 3 (we suggest k to be equal to, or bigger than, 3). Note that the
function fails if the user uses the default value but the study has set
a bigger threshold. The value of `k` is used only if the argument
`method` is set to `'deterministic'`. Any value of `k` is ignored if the
argument `method` is set to `'probabilistic'` or `'smallCellsRule'`.

The value of `noise` is used only if the argument `method` is set to
`'probabilistic'`. Any value of `noise` is ignored if the argument
`method` is set to `'deterministic'` or `'smallCellsRule'`. The user can
choose any value for `noise` equal to or greater than the pre-specified
threshold `'nfilter.noise'`.

Server function called: `heatmapPlotDS`

## Author

DataSHIELD Development Team

## Examples
