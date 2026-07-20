# Generates a histogram plot

`ds.histogram` function plots a non-disclosive histogram in the
client-side.

## Usage

``` r
ds.histogram(
  x = NULL,
  type = "split",
  num.breaks = 10,
  method = "smallCellsRule",
  k = 3,
  noise = 0.25,
  vertical.axis = "Frequency",
  datasources = NULL
)
```

## Arguments

- x:

  a character string specifying the name of a numerical vector.

- type:

  a character string that represents the type of graph to display. The
  `type` argument can be set as `'combine'` or `'split'`. Default
  `'split'`. For more information see **Details**.

- num.breaks:

  a numeric specifying the number of breaks of the histogram. Default
  value is `10`.

- method:

  a character string that defines which histogram will be created. The
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

- vertical.axis, :

  a character string that defines what is shown in the vertical axis of
  the plot. The `vertical.axis` argument can be set as `'Frequency'` or
  `'Density'`. Default `'Frequency'`. For more information see
  **Details**.

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the `datasources` argument is not
  specified the default set of connections will be used: see
  [`datashield.connections_default`](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

## Value

one or more histogram objects and plots depending on the argument `type`

## Details

`ds.histogram` function allows the user to plot distinct histograms (one
for each study) or a combined histogram that merges the single plots.

In the argument `type` can be specified two types of graphics to
display:

- `'combine'`:

  : a histogram that merges the single plot is displayed.

- `'split'`:

  : each histogram is plotted separately.

In the argument `method` can be specified 3 different histograms to be
created:

- `'smallCellsRule'`:

  : the histogram of the actual variable is created but bins with low
  counts are removed.

- `'deterministic'`:

  : the histogram of the scaled centroids of each `k` nearest neighbours
  of the original variable where the value of `k` is set by the user.

- `'probabilistic'`:

  : the histogram shows the original distribution disturbed by the
  addition of random stochastic noise. The added noise follows a normal
  distribution with zero mean and variance equal to a percentage of the
  initial variance of the input variable. This percentage is specified
  by the user in the argument `noise`.

In the `k` argument the user can choose any value for `k` equal to or
greater than the pre-specified threshold used as a disclosure control
for this method and lower than the number of observations minus the
value of this threshold. By default the value of `k` is set to be equal
to 3 (we suggest k to be equal to, or bigger than, 3). Note that the
function fails if the user uses the default value but the study has set
a bigger threshold. The value of `k` is used only if the argument
`method` is set to `'deterministic'`. Any value of k is ignored if the
argument `method` is set to `'probabilistic'` or `'smallCellsRule'`.

In the `noise` argument the percentage of the initial variance that is
used as the variance of the embedded noise if the argument `method` is
set to `'probabilistic'`. Any value of noise is ignored if the argument
`method` is set to `'deterministic'` or `'smallCellsRule'`. The user can
choose any value for noise equal to or greater than the pre-specified
threshold `'nfilter.noise'`. By default the value of noise is set to be
equal to 0.25.

In the argument `vertical.axis` can be specified two types of
histograms:

- `'Frequency'`:

  : the histogram of the frequencies is returned.

- `'Density'`:

  : the histogram of the densities is returned.

Server function called: `histogramDS2`

## Author

DataSHIELD Development Team

## Examples
