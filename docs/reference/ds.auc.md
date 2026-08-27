# Calculates the Area under the curve (AUC)

This function calculates the C-statistic or AUC for logistic regression
models.

## Usage

``` r
ds.auc(pred = NULL, y = NULL, datasources = NULL)
```

## Arguments

- pred:

  the name of the vector of the predicted values

- y:

  the name of the outcome variable. Note that this variable should
  include the complete cases that are used in the regression model.

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the `datasources` argument is not
  specified the default set of connections will be used: see
  [`datashield.connections_default`](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

## Value

returns the AUC and its standard error

## Details

The AUC determines the discriminative ability of a model.

## Author

Demetris Avraam for DataSHIELD Development Team
