# Fits Linear Mixed-Effect model via Study-Level Meta-Analysis

`ds.lmerSLMA` fits a Linear Mixed-Effects Model (lme) - can include both
fixed and random-effects - on data from one or multiple sources with
pooling via SLMA (Study-Level Meta-Analysis)

## Usage

``` r
ds.lmerSLMA(
  formula = NULL,
  offset = NULL,
  weights = NULL,
  combine.with.metafor = TRUE,
  dataName = NULL,
  checks = FALSE,
  datasources = NULL,
  REML = TRUE,
  control_type = NULL,
  control_value = NULL,
  optimizer = NULL,
  verbose = 0,
  notify.of.progress = FALSE,
  assign = FALSE,
  newobj = NULL
)
```

## Arguments

- formula:

  an object of class formula describing the model to be fitted. For more
  information see **Details**.

- offset:

  a character string specifying the name of a variable to be used as an
  offset.

- weights:

  a character string specifying the name of a variable containing prior
  regression weights for the fitting process.

- combine.with.metafor:

  logical. If TRUE the estimates and standard errors for each regression
  coefficient are pooled across studies using random-effects
  meta-analysis under maximum likelihood (ML), restricted maximum
  likelihood (REML) or fixed-effects meta-analysis (FE). Default TRUE.

- dataName:

  a character string specifying the name of an (optional) data frame
  that contains all of the variables in the LME formula. For more
  information see **Details**.

- checks:

  logical. If TRUE `ds.lmerSLMA` checks the structural integrity of the
  model. Default FALSE. For more information see **Details**.

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the `datasources` argument is not
  specified the default set of connections will be used: see
  [`datashield.connections_default`](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

- REML:

  logical. If TRUE the REstricted Maximum Likelihood (REML) is used for
  parameter optimization. If FALSE the parameters are optimized using
  standard ML (maximum likelihood). Default TRUE. For more information
  see **Details**.

- control_type:

  an optional character string vector specifying the nature of a
  parameter (or parameters) to be modified in the
  `convergence control options` which can be viewed or modified via the
  `lmerControl` function of the package `lme4`. For more information see
  **Details**.

- control_value:

  numeric representing the new value which you want to allocate the
  control parameter corresponding to the `control-type`. For more
  information see **Details**.

- optimizer:

  specifies the parameter optimizer that `lmer` should use. For more
  information see **Details**.

- verbose:

  an integer value. If \\verbose \> 0\\ the output is generated during
  the optimization of the parameter estimates. If \\verbose \> 1\\ the
  output is generated during the individual penalized iteratively
  reweighted least squares (PIRLS) steps. Default `verbose` value is 0
  which means no additional output.

- notify.of.progress:

  specifies if console output should be produced to indicate progress.
  Default FALSE.

- assign:

  a logical, indicates whether the function will call a second
  server-side function (an assign) in order to save the regression
  outcomes (i.e. a lmerMod object) on each server. Default FALSE.

- newobj:

  a character string specifying the name of the object to which the
  lmerMod object representing the model fit on the serverside in each
  study is to be written. This argument is used only when the argument
  `assign` is set to TRUE. If no \<newobj\> argument is specified, the
  output object defaults to "new.lmer.obj".

## Value

Many of the elements of the output list returned by `ds.lmerSLMA` are
equivalent to those returned by the `lmer()` function in native R.
However, potentially disclosive elements such as individual-level
residuals and linear predictor values are blocked. In this case, only
non-disclosive elements are returned from each study separately.

The list of elements returned by `ds.lmerSLMA` is mentioned below:

`ds.lmerSLMA` returns a list of elements mentioned below separately for
each study.

`coefficients`: a matrix with 5 columns:

- First:

  : the names of all of the regression parameters (coefficients) in the
  model

- second:

  : the estimated values

- third:

  : corresponding standard errors of the estimated values

- fourth:

  : the ratio of estimate/standard error

- fifth:

  : the p-value treating that as a standardised normal deviate

`CorrMatrix`: the correlation matrix of parameter estimates.

`VarCovMatrix`: the variance-covariance matrix of parameter estimates.

`weights`: the vector (if any) holding regression weights.

`offset`: the vector (if any) holding an offset.

`cov.scaled`: equivalent to `VarCovMatrix`.

`Nmissing`: the number of missing observations in the given study.

`Nvalid`: the number of valid (non-missing) observations in the given
study.

`Ntotal`: the total number of observations in the given study
(`Nvalid` + `Nmissing`).

`data`: equivalent to input parameter `dataName` (above).

`call`: summary of key elements of the call to fit the model.

There are a small number of more esoteric items of the information
returned by `ds.lmerSLMA`. Additional information about these can be
found in the help file for the `lmer()` function in the `lme4` package.

Once the study-specific output has been returned, the function returns
several elements relating to the pooling of estimates across studies via
study-level meta-analysis. These are as follows:

`input.beta.matrix.for.SLMA`: a matrix containing the vector of
coefficient estimates from each study.

`input.se.matrix.for.SLMA`: a matrix containing the vector of standard
error estimates for coefficients from each study.

`SLMA.pooled.estimates`: a matrix containing pooled estimates for each
regression coefficient across all studies with pooling under SLMA via
random-effects meta-analysis under maximum likelihood (ML), restricted
maximum likelihood (REML) or via fixed-effects meta-analysis (FE).

`convergence.error.message`: reports for each study whether the model
converged. If it did not some information about the reason for this is
reported.

## Details

`ds.lmerSLMA` fits a Linear Mixed Effects Model (lme) - can include both
fixed and random effects - on data from single or multiple sources.

This function is similar to `lmer` function from `lme4` package in
native R.

When there are multiple data sources, the LME is fitted to convergence
in each data source independently. The estimates and standard errors
returned to the client-side which enable cross-study pooling using
Study-Level Meta-Analysis (SLMA). The SLMA used by default `metafor`
package but as the SLMA occurs on the client-side (a standard R
environment), the user can choose any approach to meta-analysis.
Additional information about fitting LMEs using the `lmer` function can
be obtained using R help for `lmer` and the `lme4` package.

In `formula` most shortcut notation allowed by `lmer()` function is also
allowed by `ds.lmerSLMA`. Many LMEs can be fitted very simply using a
formula like: \\y ~ a + b + (1 \| c)\\ which simply means fit an LME
with `y` as the outcome variable with `a` and `b` as fixed effects, and
`c` as a random effect or grouping factor.

It is also possible to fit models with random slopes by specifying a
model such as \\y ~ a + b + (1 + b \| c)\\ where the effect of `b` can
vary randomly between groups defined by `c`. Implicit nesting can be
specified with formulae such as \\y ~ a + b + (1 \| c / d)\\ or \\y ~
a + b + (1 \| c) + (1 \| c : d)\\.

The `dataName` argument avoids you having to specify the name of the
data frame in front of each covariate in the formula. For example, if
the data frame is called `DataFrame` you avoid having to write:
\\DataFrame\\y ~ DataFrame\\a + DataFrame\\b + (1 \| DataFrame\\c)\\.

The `checks` argument verifies that the variables in the model are all
defined (exist) on the server-site at every study and that they have the
correct characteristics required to fit the model. It is suggested to
make `checks` argument TRUE if an unexplained problem in the model fit
is encountered because the running process takes several minutes.

`REML` can help to mitigate bias associated with the fixed-effects. See
help on the `lmer()` function for more details.

In `control_type` at present only one such parameter can be modified,
namely the tolerance of the convergence criterion to the gradient of the
log-likelihood at the maximum likelihood achieved. We have enabled this
because our practical experience suggests that in situations where the
model looks to have converged with sensible parameter values but formal
convergence is not being declared if we allow the model to be more
tolerant to a non-zero gradient the same parameter values are obtained
but formal convergence is declared. The default value for the
`check.conv.grad` is `0.002`.

`control_value` At present (see `control_type`) the only parameter this
can be is the convergence tolerance `check.conv.grad`. In general,
models will be identified as having converged more readily if the value
set for `check.conv.grad` is increased from its default (`0.002`).
Please note that the risk of doing this is that the model is also more
likely to be declared as having converged at a local maximum that is not
the global maximum likelihood. This will not generally be a problem if
the likelihood surface is well behaved but if you have a problem with
convergence you might usefully compare all the parameter estimates and
standard errors obtained using the default tolerance (`0.002`) even
though that has not formally converged with those obtained after
convergence using the higher tolerance.

The `optimizer` argument is built in but it won't do anything because
there is only one standard optimizer available for lmer - this is the
`nloptwrap` optimizer. If users wish to apply a different optimizer -
potentially one they have developed themselves - the development team
can activate this argument so alternatives can be specified.

Server function called: `lmerSLMADS2`

## Author

DataSHIELD Development Team

## Examples

``` r
if (FALSE) { # \dontrun{

 ## Version 6, for version 5 see Wiki
  # Connecting to the Opal servers
  
  require('DSI')
  require('DSOpal')
  require('dsBaseClient')
  
  builder <- DSI::newDSLoginBuilder()
  builder$append(server = "study1", 
                 url = "http://192.168.56.100:8080/", 
                 user = "administrator", password = "datashield_test&", 
                 table = "CLUSTER.CLUSTER_SLO1", driver = "OpalDriver")
  builder$append(server = "study2", 
                 url = "http://192.168.56.100:8080/", 
                 user = "administrator", password = "datashield_test&", 
                 table = "CLUSTER.CLUSTER_SLO2", driver = "OpalDriver")
  builder$append(server = "study3",
                 url = "http://192.168.56.100:8080/", 
                 user = "administrator", password = "datashield_test&", 
                 table = "CLUSTER.CLUSTER_SLO3", driver = "OpalDriver")
  logindata <- builder$build()
  
   #Log onto the remote Opal training servers
   connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D")
               
  # Select all rows without missing values
  ds.completeCases(x1 = "D", newobj = "D.comp", datasources = connections)
 
  # Fit the lmer
  
  ds.lmerSLMA(formula = "BMI ~ incid_rate + diabetes + (1 | Male)",
               dataName = "D.comp",
               datasources = connections)
  
  # Clear the Datashield R sessions and logout
  datashield.logout(connections) 
  } # }
```
