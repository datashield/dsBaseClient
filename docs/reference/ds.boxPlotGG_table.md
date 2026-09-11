# Draw boxplot with information from a data frame

Draws a boxplot with the option of adding two grouping variables from
data held on a table

## Usage

``` r
ds.boxPlotGG_table(
  x,
  variables,
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

  `character` Name of the table on the server side that holds the
  information to be plotted

- variables:

  `character vector` Name of the column(s) of the data frame to include
  on the boxplot

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
