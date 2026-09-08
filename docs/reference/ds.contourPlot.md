# Generates a contour plot

It generates a contour plot of the pooled data or one plot for each
dataset on the client-side.

## Usage

``` r
ds.contourPlot(
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

  a character string providing the name of a numerical vector.

- y:

  a character string providing the name of a numerical vector.

- type:

  a character string that represents the type of graph to display. If
  `type` is set to `'combine'`, a combined contour plot displayed and if
  `type` is set to `'split'`, each contour is plotted separately.

- show:

  a character that represents where the plot should focus. If `show` is
  set to `'all'`, the ranges of the variables are used as plot limits.
  If `show` is set to `'zoomed'`, the plot is zoomed to the region where
  the actual data are.

- numints:

  number of intervals for a density grid object.

- method:

  a character that defines which contour will be created. If `method` is
  set to `'smallCellsRule'` (default), the contour plot of the actual
  variables is created but grids with low counts are replaced with grids
  with zero counts. If `method` is set to `'deterministic'` the contour
  of the scaled centroids of each k nearest neighbour of the original
  variables is created, where the value of `k` is set by the user. If
  the `method` is set to `'probabilistic'`, then the contour of 'noisy'
  variables is generated.

- k:

  the number of the nearest neighbours for which their centroid is
  calculated. For more information see details.

- noise:

  the percentage of the initial variance that is used as the variance of
  the embedded noise if the argument `method` is set to
  `'probabilistic'`. For more information see details.

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the `datasources` argument is not
  specified the default set of connections will be used: see
  [`datashield.connections_default`](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

## Value

`ds.contourPlot` returns a contour plot to the client-side.

## Details

The `ds.contourPlot` function first generates a density grid and uses it
to plot the graph. The cells of the grid density matrix that hold a
count of less than the filter set by DataSHIELD (usually 5) are
considered invalid and turned into 0 to avoid potential disclosure. A
message is printed to inform the user about the number of invalid cells.

The ranges returned by each study and used in the process of getting the
grid density matrix are not the exact minimum and maximum values but
rather close approximates of the real minimum and maximum value. This
was done to reduce the risk of potential disclosure.

In the `k` parameter the user can choose any value for `k` equal to or
greater than the pre-specified threshold used as a disclosure control
for this method and lower than the number of observations minus the
value of this threshold. `k` default value is 3 (we suggest k to be
equal to, or bigger than, 3). Note that the function fails if the user
uses the default value but the study has set a bigger threshold. The
value of `k` is used only if the argument `method` is set to
`'deterministic'`. Any value of k is ignored if the argument `method` is
set to `'probabilistic'` or `'smallCellsRule'`.

In `noise` any value of noise is ignored if the argument `method` is set
to `'deterministic'` or `'smallCellsRule'`. The user can choose any
value for noise equal to or greater than the pre-specified threshold
`'nfilter.noise'`. Default noise value is 0.25. The added noise follows
a normal distribution with zero mean and variance equal to a percentage
of the initial variance of each input variable.

Server functions called: `heatmapPlotDS`, `rangeDS` and `densityGridDS`

## Author

DataSHIELD Development Team

## Examples

``` r
if (FALSE) { # \dontrun{

  ## Version 6, for version 5 see the Wiki
  # Connecting to the Opal servers

  require('DSI')
  require('DSOpal')
  require('dsBaseClient')

  builder <- DSI::newDSLoginBuilder()
  builder$append(server = "study1", 
                 url = "http://192.168.56.100:8080/", 
                 user = "administrator", password = "datashield_test&", 
                 table = "CNSIM.CNSIM1", driver = "OpalDriver")
  builder$append(server = "study2", 
                 url = "http://192.168.56.100:8080/", 
                 user = "administrator", password = "datashield_test&", 
                 table = "CNSIM.CNSIM2", driver = "OpalDriver")
  builder$append(server = "study3",
                 url = "http://192.168.56.100:8080/", 
                 user = "administrator", password = "datashield_test&", 
                 table = "CNSIM.CNSIM3", driver = "OpalDriver")
  logindata <- builder$build()
  
  # Log onto the remote Opal training servers
  connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D") 
  
  # Generating contour plots

  ds.contourPlot(x = "D$LAB_TSC",
                 y = "D$LAB_HDL",
                 type = "combine", 
                 show = "all",
                 numints = 20,
                 method = "smallCellsRule",  
                 k = 3, 
                 noise = 0.25,
                 datasources = connections)

  # clear the Datashield R sessions and logout
  datashield.logout(connections)

} # }
```
