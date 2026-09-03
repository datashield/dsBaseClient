# Take a data frame on the server side an arrange it to pass it to the boxplot function

Internal function

## Usage

``` r
ds.boxPlotGG_data_Treatment(
  table,
  variables,
  group = NULL,
  group2 = NULL,
  datasources = NULL
)
```

## Arguments

- table:

  `character` Name of the table on the server side that holds the
  information to be plotted later

- variables:

  `character vector` Name of the column(s) of the data frame to include
  on the boxplot

- group:

  `character` (default `NULL`) Name of the first grouping variable.

- group2:

  `character` (default `NULL`) Name of the second grouping variable.

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  (default `NULL`) objects obtained after login

## Value

Does not return nothing, it creates the table `"boxPlotRawData"` on the
server arranged to be passed to the ggplot boxplot function. Structure
of the created table:  

Column 'x': Names on the X axis of the boxplot, aka variables to plot  
Column 'value': Values for that variable (raw data of columns rbinded)  
Column 'group': (Optional) Values of the grouping variable  
Column 'group2': (Optional) Values of the second grouping variable  
