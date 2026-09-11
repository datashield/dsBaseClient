# Calculates the kurtosis of a numeric variable

This function calculates the kurtosis of a numeric variable.

## Usage

``` r
ds.kurtosis(
  x = NULL,
  method = 1,
  type = "both",
  datasources = NULL,
  classConsistencyCheck = FALSE
)
```

## Arguments

- x:

  a string character, the name of a numeric variable.

- method:

  an integer between 1 and 3 selecting one of the algorithms for
  computing kurtosis detailed below. The default value is set to 1.

- type:

  a character which represents the type of analysis to carry out. If
  `type` is set to 'combine', 'combined', 'combines' or 'c', the global
  kurtosis is returned if `type` is set to 'split', 'splits' or 's', the
  kurtosis is returned separately for each study. if `type` is set to
  'both' or 'b', both sets of outputs are produced. The default value is
  set to 'both'.

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the `datasources` argument is not
  specified the default set of connections will be used: see
  [`datashield.connections_default`](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

- classConsistencyCheck:

  logical. If TRUE, checks that the input object has the same class
  across all studies. Default FALSE.

## Value

a matrix showing the kurtosis of the input numeric variable and the
number of valid observations.

## Details

The function calculates the kurtosis of an input variable x with three
different methods. The method is specified by the argument `method`. If
x contains any missings, the function removes those before the
calculation of the kurtosis. If `method` is set to 1 the following
formula is used \\ kurtosis= \frac{\sum\_{i=1}^{N} (x_i - \bar(x))^4
/N}{(\sum\_{i=1}^{N} ((x_i - \bar(x))^2) /N)^(2) } - 3\\, where \\
\bar{x} \\ is the mean of x and \\N\\ is the number of observations. If
`method` is set to 2 the following formula is used \\ kurtosis=
((N+1)\*(\frac{\sum\_{i=1}^{N} (x_i - \bar(x))^4 /N}{(\sum\_{i=1}^{N}
((x_i - \bar(x))^2) /N)^(2) } - 3) + 6)\*((N-1)/((N-2)\*(N-3)))\\. If
`method` is set to 3 the following formula is used \\ kurtosis=
(\frac{\sum\_{i=1}^{N} (x_i - \bar(x))^4 /N}{(\sum\_{i=1}^{N} ((x_i -
\bar(x))^2) /N)^(2) })\*(1-1/N)^2 - 3\\. This function is similar to the
function `kurtosis` in R package `e1071`.

## Author

Demetris Avraam, for DataSHIELD Development Team

Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
