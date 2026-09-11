# Take a vector on the server side an arrange it to pass it to the boxplot function

Internal function

## Usage

``` r
ds.boxPlotGG_data_Treatment_numeric(vector, datasources = NULL)
```

## Arguments

- vector:

  `character` Name of the table on the server side that holds the
  information to be plotted later

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  (default `NULL`) objects obtained after login

## Value

Does not return nothing, it creates the table `"boxPlotRawDataNumeric"`
on the server arranged to be passed to the ggplot boxplot function.
Structure of the created table:  

Column 'x': Names on the X axis of the boxplot, aka name of the vector
(vector argument)  
Column 'value': Values for that variable  
