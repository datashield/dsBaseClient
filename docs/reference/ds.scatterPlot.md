# Generates non-disclosive scatter plots

This function uses two disclosure control methods to generate
non-disclosive scatter plots of two server-side continuous variables.

## Usage

``` r
ds.scatterPlot(
  x = NULL,
  y = NULL,
  method = "deterministic",
  k = 3,
  noise = 0.25,
  type = "split",
  return.coords = FALSE,
  datasources = NULL
)
```

## Arguments

- x:

  a character string specifying the name of the explanatory variable, a
  numeric vector.

- y:

  a character string specifying the name of the response variable, a
  numeric vector.

- method:

  a character string that specifies the method that is used to generated
  non-disclosive coordinates to be displayed in a scatter plot. This
  argument can be set as `'deteministic'` or `'probabilistic'`. Default
  `'deteministic'`. For more information see **Details**.

- k:

  the number of the nearest neighbors for which their centroid is
  calculated. Default 3. For more information see **Details**.

- noise:

  the percentage of the initial variance that is used as the variance of
  the embedded noise if the argument `method` is set to
  `'probabilistic'`. For more information see **Details**.

- type:

  a character that represents the type of graph to display. This can be
  set as `'combine'` or `'split'`. Default `'split'`. For more
  information see **Details**.

- return.coords:

  a logical. If TRUE the coordinates of the anonymised data points are
  return to the Console. Default value is FALSE.

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the `datasources` argument is not
  specified the default set of connections will be used: see
  [`datashield.connections_default`](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

## Value

`ds.scatterPlot` returns to the client-side one or more scatter plots
depending on the argument `type`.

## Details

As the generation of a scatter plot from original data is disclosive and
is not permitted in DataSHIELD, this function allows the user to plot
non-disclosive scatter plots.

If the argument `method` is set to `'deterministic'`, the server-side
function searches for the `k-1` nearest neighbors of each single data
point and calculates the centroid of such `k` points. The proximity is
defined by the minimum Euclidean distances of z-score transformed data.

When the coordinates of all centroids are estimated the function applies
scaling to expand the centroids back to the dispersion of the original
data. The scaling is achieved by multiplying the centroids with a
scaling factor that is equal to the ratio between the standard deviation
of the original variable and the standard deviation of the calculated
centroids. The coordinates of the scaled centroids are then returned to
the client-side.

The value of `k` is specified by the user. The suggested and default
value is equal to 3 which is also the suggested minimum threshold that
is used to prevent disclosure which is specified in the protection
filter `nfilter.kNN`. When the value of `k` increases, the disclosure
risk decreases but the utility loss increases. The value of `k` is used
only if the argument `method` is set to `'deterministic'`. Any value of
`k` is ignored if the argument `method` is set to `'probabilistic'`.

If the argument `method` is set to `'probabilistic'`, the server-side
function generates a random normal noise of zero mean and variance equal
to 10% of the variance of each `x` and `y` variable. The noise is added
to each `x` and `y` variable and the disturbed by the addition of
`noise` data are returned to the client-side. Note that the seed random
number generator is fixed to a specific number generated from the data
and therefore the user gets the same figure every time that chooses the
probabilistic method in a given set of variables. The value of `noise`
is used only if the argument `method` is set to `'probabilistic'`. Any
value of `noise` is ignored if the argument `method` is set to
`'deterministic'`.

In `type` argument can be set two graphics to display:  
(1) If `type = 'combine'` a scatter plot for combined data is
generated.  
(2) If `type = 'split'` one scatter plot for each study is generated.

Server function called: `scatterPlotDS`

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

  #Example 1: generate a scatter plot for each study separately
  #Using the default deterministic method and k = 10
  
  ds.scatterPlot(x = "D$PM_BMI_CONTINUOUS",
                 y = "D$LAB_GLUC_ADJUSTED",
                 method = "deterministic",
                 k = 10,
                 type = "split",
                 datasources = connections)

  #Example 2: generate a combined scatter plot with the probabilistic method
  #and noise of variance 0.5% of the variable's variance, and display the coordinates
  # of the anonymised data points to the Console
  
  ds.scatterPlot(x = "D$PM_BMI_CONTINUOUS",
                 y = "D$LAB_GLUC_ADJUSTED",
                 method = "probabilistic",
                 noise = 0.5,
                 type = "combine",
                 datasources = connections)
                   
  #Clear the Datashield R sessions and logout
  datashield.logout(connections) 

} # }
```
