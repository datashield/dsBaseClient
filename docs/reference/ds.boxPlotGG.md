# Renders boxplot

Internal function. Renders a ggplot boxplot by retrieving from the
server side a list with the identity stats and other parameters to
render the plot without passing any data from the original dataset

## Usage

``` r
ds.boxPlotGG(
  x,
  group = NULL,
  group2 = NULL,
  xlabel = "x axis",
  ylabel = "y axis",
  type = "pooled",
  datasources = NULL
)
```

## Arguments

- x:

  `character` Name on the server side of the data frame to form a
  boxplot. Structure on the server of this object must be:  

  Column 'x': Names on the X axis of the boxplot, aka variables to
  plot  
  Column 'value': Values for that variable (raw data of columns
  rbinded)  
  Column 'group': (Optional) Values of the grouping variable  
  Column 'group2': (Optional) Values of the second grouping variable  

- group:

  `character` (default `NULL`) Name of the first grouping variable.

- group2:

  `character` (default `NULL`) Name of the second grouping variable.

- xlabel:

  `caracter` (default `"x axis"`) Label to put on the x axis of the plot

- ylabel:

  `caracter` (default `"y axis"`) Label to put on the y axis of the plot

- type:

  `character` Return a pooled plot (`"pooled"`) or a split plot (one for
  each study server `"split"`)

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  (default `NULL`) objects obtained after login

## Value

`ggplot` object
