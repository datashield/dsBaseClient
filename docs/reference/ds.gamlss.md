# Generalized Additive Models for Location Scale and Shape

This function calls the gamlssDS that is a wrapper function from the
gamlss R package. The function returns an object of class "gamlss",
which is a generalized additive model for location, scale and shape
(GAMLSS). The function also saves the residuals as an object on the
server-side with a name specified by the newobj argument. In addition,
if the argument centiles is set to TRUE, the function calls the centiles
function from the gamlss package and returns the sample percentages
below each centile curve.

## Usage

``` r
ds.gamlss(
  formula = NULL,
  sigma.formula = "~1",
  nu.formula = "~1",
  tau.formula = "~1",
  family = "NO()",
  data = NULL,
  method = "RS",
  mu.fix = FALSE,
  sigma.fix = FALSE,
  nu.fix = FALSE,
  tau.fix = FALSE,
  control = c(0.001, 20, 1, 1, 1, 1, Inf),
  i.control = c(0.001, 50, 30, 0.001),
  centiles = FALSE,
  xvar = NULL,
  newobj = NULL,
  datasources = NULL
)
```

## Arguments

- formula:

  a formula object, with the response on the left of an ~ operator, and
  the terms, separated by + operators, on the right. Nonparametric
  smoothing terms are indicated by pb() for penalised beta splines, cs
  for smoothing splines, lo for loess smooth terms and random or ra for
  random terms, e.g. 'y~cs(x,df=5)+x1+x2\*x3'.

- sigma.formula:

  a formula object for fitting a model to the sigma parameter, as in the
  formula above, e.g. sigma.formula='~cs(x,df=5)'.

- nu.formula:

  a formula object for fitting a model to the nu parameter, e.g.
  nu.formula='~x'.

- tau.formula:

  a formula object for fitting a model to the tau parameter, e.g.
  tau.formula='~cs(x,df=2)'.

- family:

  a gamlss.family object, which is used to define the distribution and
  the link functions of the various parameters. The distribution
  families supported by gamlss() can be found in gamlss.family.
  Functions such as 'BI()' (binomial) produce a family object. Also can
  be given without the parentheses i.e. 'BI'. Family functions can take
  arguments, as in 'BI(mu.link=probit)'.

- data:

  a data frame containing the variables occurring in the formula. If
  this is missing, the variables should be on the parent environment.

- method:

  a character indicating the algorithm for GAMLSS. Can be either 'RS',
  'CG' or 'mixed'. If method='RS' the function will use the Rigby and
  Stasinopoulos algorithm, if method='CG' the function will use the Cole
  and Green algorithm, and if method='mixed' the function will use the
  RS algorithm twice before switching to the Cole and Green algorithm
  for up to 10 extra iterations.

- mu.fix:

  logical, indicate whether the mu parameter should be kept fixed in the
  fitting processes.

- sigma.fix:

  logical, indicate whether the sigma parameter should be kept fixed in
  the fitting processes.

- nu.fix:

  logical, indicate whether the nu parameter should be kept fixed in the
  fitting processes.

- tau.fix:

  logical, indicate whether the tau parameter should be kept fixed in
  the fitting processes.

- control:

  this sets the control parameters of the outer iterations algorithm
  using the gamlss.control function. This is a vector of 7 numeric
  values: (i) c.crit (the convergence criterion for the algorithm), (ii)
  n.cyc (the number of cycles of the algorithm), (iii) mu.step (the step
  length for the parameter mu), (iv) sigma.step (the step length for the
  parameter sigma), (v) nu.step (the step length for the parameter
  nu), (vi) tau.step (the step length for the parameter tau), (vii)
  gd.tol (global deviance tolerance level). The default values for these
  7 parameters are set to c(0.001, 20, 1, 1, 1, 1, Inf).

- i.control:

  this sets the control parameters of the inner iterations of the RS
  algorithm using the glim.control function. This is a vector of 4
  numeric values: (i) cc (the convergence criterion for the
  algorithm), (ii) cyc (the number of cycles of the algorithm), (iii)
  bf.cyc (the number of cycles of the backfitting algorithm), (iv)
  bf.tol (the convergence criterion (tolerance level) for the
  backfitting algorithm). The default values for these 4 parameters are
  set to c(0.001, 50, 30, 0.001).

- centiles:

  logical, indicating whether the function centiles() will be used to
  tabulate the sample percentages below each centile curve. Default is
  set to FALSE.

- xvar:

  the unique explanatory variable used in the centiles() function. This
  variable is used only if the centiles argument is set to TRUE. A
  restriction in the centiles function is that it applies to models with
  one explanatory variable only.

- newobj:

  a character string that provides the name for the output object that
  is stored on the data servers. Default `gamlss_res`.

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the `datasources` argument is not
  specified the default set of connections will be used: see
  [`datashield.connections_default`](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

## Value

a gamlss object with all components as in the native R gamlss function.
Individual-level information like the components y (the response
response) and residuals (the normalised quantile residuals of the model)
are not disclosed to the client-side.

## Details

For additional details see the help header of gamlss and centiles
functions in native R gamlss package.

## Author

Demetris Avraam for DataSHIELD Development Team
