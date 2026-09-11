# Draw boxplot with information from a numeric vector

Draw boxplot with information from a numeric vector

## Usage

``` r
ds.boxPlotGG_numeric(
  x,
  xlabel = "x axis",
  ylabel = "y axis",
  type = "pooled",
  datasources = NULL
)
```

## Arguments

- x:

  `character` Name of the numeric vector on the server side that holds
  the information to be plotted

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
