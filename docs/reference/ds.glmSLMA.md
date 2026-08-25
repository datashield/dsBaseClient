# Fit a Generalized Linear Model (GLM) with pooling via Study Level Meta-Analysis (SLMA)

Fits a generalized linear model (GLM) on data from single or multiple
sources with pooled co-analysis across studies being based on SLMA
(Study Level Meta Analysis).

## Usage

``` r
ds.glmSLMA(
  formula = NULL,
  family = NULL,
  offset = NULL,
  weights = NULL,
  combine.with.metafor = TRUE,
  newobj = NULL,
  dataName = NULL,
  checks = FALSE,
  maxit = 30,
  notify.of.progress = FALSE,
  datasources = NULL
)
```

## Arguments

- formula:

  an object of class formula describing the model to be fitted. For more
  information see **Details**.

- family:

  identifies the error distribution function to use in the model.

- offset:

  a character string specifying the name of a variable to be used as an
  offset.`ds.glmSLMA` does not allow an offset vector to be written
  directly into the GLM formula.

- weights:

  a character string specifying the name of a variable containing prior
  regression weights for the fitting process. `ds.glmSLMA` does not
  allow a weights vector to be written directly into the GLM formula.

- combine.with.metafor:

  logical. If TRUE the estimates and standard errors for each regression
  coefficient are pooled across studies using random-effects
  meta-analysis under maximum likelihood (ML), restricted maximum
  likelihood (REML) or fixed-effects meta-analysis (FE). Default TRUE.

- newobj:

  a character string specifying the name of the object to which the glm
  object representing the model fit on the serverside in each study is
  to be written. If no \<newobj\> argument is specified, the output
  object defaults to "new.glm.obj".

- dataName:

  a character string specifying the name of an (optional) data frame
  that contains all of the variables in the GLM formula.

- checks:

  logical. If TRUE `ds.glmSLMA` checks the structural integrity of the
  model. Default FALSE. For more information see **Details**.

- maxit:

  a numeric scalar denoting the maximum number of iterations that are
  permitted before `ds.glmSLMA` declares that the model has failed to
  converge. For more information see **Details**.

- notify.of.progress:

  specifies if console output should be produced to indicate progress.
  Default FALSE.

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the `datasources` argument is not
  specified the default set of connections will be used: see
  [`datashield.connections_default`](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

## Value

The serverside aggregate functions `glmSLMADS1` and `glmSLMADS2` return
output to the clientside, while the assign function `glmSLMADS.assign`
simply writes the glm object to the serverside created by the model fit
on a given server as a permanent object on that same server. This is
precisely the same as the glm object that is usually created by a call
to glm() in native R and it contains all the same elements (see help for
glm in native R). Because it is a serverside object, no disclosure
blocks apply. However, such disclosure blocks do apply to the
information passed to the clientside. In consequence, rather than
containing all the components of a standard glm object in native R, the
components of the glm object that are returned by `ds.glmSLMA` include:
a mixture of non-disclosive elements of the glm object reported
separately by study included in a list object called `output.summary`;
and a series of other list objects that represent inferences aggregated
across studies.

the study specific items include:

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

`family`: indicates the error distribution and link function used in the
GLM.

`formula`: model formula, see description of formula as an input
parameter (above).

`df.resid`: the residual degrees of freedom around the model.

`deviance.resid`: the residual deviance around the model.

`df.null`: the degrees of freedom around the null model (with just an
intercept).

`dev.null`: the deviance around the null model (with just an intercept).

`CorrMatrix`: the correlation matrix of parameter estimates.

`VarCovMatrix`: the variance-covariance matrix of parameter estimates.

`weights`: the name of the vector (if any) holding regression weights.

`offset`: the name of the vector (if any) holding an offset (enters glm
with a coefficient of 1.00).

`cov.scaled`: equivalent to `VarCovMatrix`.

`cov.unscaled`: equivalent to VarCovMatrix but assuming dispersion
(scale) parameter is 1.

`Nmissing`: the number of missing observations in the given study.

`Nvalid`: the number of valid (non-missing) observations in the given
study.

`Ntotal`: the total number of observations in the given study
(`Nvalid` + `Nmissing`).

`data`: equivalent to input parameter `dataName` (above).

`dispersion`: the estimated dispersion parameter:
deviance.resid/df.resid for a gaussian family multiple regression model,
1.00 for logistic and poisson regression.

`call`: summary of key elements of the call to fit the model.

`na.action`: chosen method of dealing with missing values. This is
usually, `na.action = na.omit` - see help in native R.

`iter`: the number of iterations required to achieve convergence of the
glm model in each separate study.

Once the study-specific output has been returned, `ds.glmSLMA` returns a
series of lists relating to the aggregated inferences across studies.
These include the following:

`num.valid.studies`: the number of studies with valid output included in
the combined analysis

`betamatrix.all`: matrix with a row for each regression coefficient and
a column for each study reporting the estimated regression coefficients
by study.

`sematrix.all`: matrix with a row for each regression coefficient and a
column for each study reporting the standard errors of the estimated
regression coefficients by study.

`betamatrix.valid`: matrix with a row for each regression coefficient
and a column for each study reporting the estimated regression
coefficients by study but only for studies with valid output (eg not
violating disclosure traps)

`sematrix.all`: matrix with a row for each regression coefficient and a
column for each study reporting the standard errors of the estimated
regression coefficients by study but only for studies with valid output
(eg not violating disclosure traps)

`SLMA.pooled.estimates.matrix`: a matrix with a row for each regression
coefficient and six columns. The first two columns contain the pooled
estimate of each regression coefficients and its standard error with
pooling via random effect meta-analysis under maximum likelihood (ML).
Columns 3 and 4 contain the estimates and standard errors from random
effect meta-analysis under REML and columns 5 and 6 the estimates and
standard errors under fixed effect meta-analysis. This matrix is only
returned if the argument combine.with.metafor is set to TRUE. Otherwise,
users can take the `betamatrix.valid` and `sematrix.valid` matrices and
enter them into their meta-analysis package of choice.

`is.object.created` and `validity.check` are standard items returned by
an assign function when the designated newobj appears to have been
successfuly created on the serverside at each study. This output is
produced specifically by the assign function `glmSLMADS.assign` that
writes out the glm object on the serverside

## Details

`ds.glmSLMA` specifies the structure of a Generalized Linear Model to be
fitted separately on each study or data source. Calls serverside
functions glmSLMADS1 (aggregate),glmSLMADS2 (aggregate) and
glmSLMADS.assign (assign). From a mathematical perspective, the SLMA
approach (using `ds.glmSLMA`) differs fundamentally from the alternative
approach using `ds.glm`. ds.glm fits the model iteratively across all
studies together. At each iteration the model in every data source has
precisely the same coefficients so when the model converges one
essentially identifies the model that best fits all studies
simultaneously. This mathematically equivalent to placing all
individual-level data from all sources in one central warehouse and
analysing those data as one combined dataset using the conventional
[`glm()`](https://rdrr.io/r/stats/glm.html) function in native R. In
contrast ds.glmSLMA sends a command to every data source to fit the
model required but each separate source simply fits that model to
completion (ie undertakes all iterations until the model converges) and
the estimates (regression coefficients) and their standard errors from
each source are sent back to the client and are then pooled using SLMA
via any approach the user wishes to implement. The ds.glmSLMA functions
includes an argument \<combine.with.metafor\> which if TRUE (the
default) pools the models across studies using the metafor function
(from the metafor package) using three optimisation methods: random
effects under maximum likelihood (ML); random effects under restricted
maximum likelihood (REML); or fixed effects (FE). But once the estimates
and standard errors are on the clientside, the user can alternatively
choose to use the metafor package in any way he/she wishes, to pool the
coefficients across studies or, indeed, to use another meta-analysis
package, or their own code.

Although the ds.glm approach might at first sight appear to be
preferable under all circumstances, this is not always the case. First,
the results from both approaches are generally very similar. Secondly,
the SLMA approach can offer key inferential advantages when there is
marked heterogeneity between sources that cannot simply be corrected by
including fixed-effects in one's ds.glm model that each reflect a study-
or centre-specific effect. In particular, such fixed effects cannot be
guaranteed to generate formal inferences that are unbiased when there is
heterogeneity in the effect that is actually of scientific interest. It
might be argued that one should not try to pool the inferences anyway if
there is marked heterogeneity, but you can use the joint analysis to
formally check for such heterogeneity and then choose to report the
pooled result or separate results from each study individually.
Crucially, unless the heterogeneity is substantial, pooling can be quite
reasonable. Furthermore, if you just fit a ds.glm model without
centre-effects you will in effect be pooling across all studies without
checking for heterogeneity and if heterogeneity exists and if it is
strong you can get theoretically results that are badly confounded by
study. Before we introduced ds.glmSLMA we encountered a real world
example of a ds.glm (without centre effects) which generated combined
inferences over all studies which were more extreme than the results
from any of the individual studies: the lower 95 of the combined
estimate was higher than the upper 95 ALL of the individual studies.
This was clearly incorrect and provided a salutary lesson on the
potential impact of confounding by study if a ds.glm model does not
include appropriate centre-effects. Even if you are going to undertake a
ds.glm analysis (which is slightly more powerful when there is no
heterogeneity) it may still be useful to also carry out a ds.glmSLMA
analysis as this provides a very easy way to examine the extent of
heterogeneity.

In `formula` Most shortcut notation for formulas allowed under R's
standard [`glm()`](https://rdrr.io/r/stats/glm.html) function is also
allowed by `ds.glmSLMA`.

Many glms can be fitted very simply using a formula such as:

\\y~a+b+c+d\\

which simply means fit a glm with `y` as the outcome variable and `a`,
`b`, `c` and `d` as covariates. By default all such models also include
an intercept (regression constant) term.

Instead, if you need to fit a more complex model, for example:

\\EVENT~1+TID+SEXF\*AGE.60\\

In the above model the outcome variable is `EVENT` and the covariates
`TID` (factor variable with level values between 1 and 6 denoting the
period time), `SEXF` (factor variable denoting sex) and `AGE.60`
(quantitative variable representing age-60 in years). The term `1`
forces the model to include an intercept term, in contrast if you use
the term `0` the intercept term is removed. The `*` symbol between
`SEXF` and `AGE.60` means fit all possible main effects and interactions
for and between those two covariates. This takes the value 0 in all
males `0 * AGE.60` and in females `1 * AGE.60`. This model is in example
1 of the section **Examples**. In this case the logarithm of the
survival time is added as an offset (`log(survtime)`).

In the `family` argument a range of model types can be fitted. This
range has recently been extended to include a number of model types that
are non-standard but are used relatively widely.

The standard models include:

- `"gaussian"`:

  : conventional linear model with normally distributed errors

- `"binomial"`:

  : conventional unconditional logistic regression model

- `"poisson"`:

  : Poisson regression model which is often used in epidemiological
  analysis of counts and rates and is also used in survival analysis.
  The Piecewise Exponential Regression (PER) model typically provides a
  close approximation to the Cox regression model in its main estimates
  and standard errors.

- `"gamma"`:

  : a family of models for outcomes characterised by a constant
  coefficient of variation, i.e. the variance increases with the square
  of the expected mean

- `"quasipoisson"`:

  : a model with a Poisson variance function - variance equals expected
  mean - but the residual variance which is fixed to be 1.00 in a
  standard Poisson model can then take any value. This is achieved by a
  dispersion parameter which is estimated during the model fit and if it
  takes the value K it means that the expected variance is K x the
  expected mean, which implies that all standard errors will be sqrt(K)
  times larger than in a standard Poisson model fitted to the same data.
  This allows for the extra uncertainty which is associated with
  'overdispersion' that occurs very commonly with Poisson distributed
  data, and typically arises when the count/rate data being modelled
  occur in blocks which exhibit heterogeneity of underlying risk which
  is not being fully modelled, either by including the blocks themselves
  as a factor or by including covariates for all the determinants that
  are relevant to that underlying risk. If there is no overdispersion
  (K=1) the estimates and standard errors from the quasipoisson model
  will be almost identical to those from a standard poisson model.

- `"quasibinomial"`:

  : a model with a binomial variance function - if P is the expected
  proportion of successes, and N is the number of "trials" (always 1 if
  analysing binary data which are formally described as having a
  Bernoulli distribution (binomial distribution with N=1) the variance
  function is `N*(P)*(1-P)`. But the residual variance which is fixed to
  be 1.00 in a binomial model can take any value. This is achieved by a
  dispersion parameter which is estimated during the model fit (see
  quasipoisson information above).

Each class of models has a "canonical link" which represents the link
function that maximises the information extraction by the model. The
gaussian family uses the `identity` link, the poisson family the `log`
link, the binomial/Bernoulli family the `logit` link and the the gamma
family the `reciprocal` link.

The `dataName` argument avoids you having to specify the name of the
data frame in front of each covariate in the formula. For example, if
the data frame is called `DataFrame` you avoid having to write:
\\DataFrame\\y ~ DataFrame\\a + DataFrame\\b + DataFrame\\c +
DataFrame\\d\\

The `checks` argument verifies that the variables in the model are all
defined (exist) on the server-site at every study and that they have the
correct characteristics required to fit the model. It is suggested to
make `checks` argument TRUE only if an unexplained problem in the model
fit is encountered because the running process takes several minutes.

In `maxit` Logistic regression and Poisson regression models can require
many iterations, particularly if the starting value of the regression
constant is far away from its actual value that the GLM is trying to
estimate. In consequence we often set `maxit=30` but depending on the
nature of the models you wish to fit, you may wish to be alerted much
more quickly than this if there is a delay in convergence, or you may
wish to allow more iterations.

Server functions called: `glmSLMADS1`, `glmSLMADS2`, `glmSLMADS.assign`

## Author

Paul Burton, for DataSHIELD Development Team 07/07/20

## Examples

``` r
if (FALSE) { # \dontrun{

 ## Version 6, for version 5 see Wiki
  # Connecting to the Opal servers
  
  require('DSI')
  require('DSOpal')
  require('dsBaseClient')
  
  # Example 1: Fitting GLM for survival analysis
  # For this analysis we need to load survival data from the server 
  
  builder <- DSI::newDSLoginBuilder()
  builder$append(server = "study1", 
                 url = "http://192.168.56.100:8080/", 
                 user = "administrator", password = "datashield_test&", 
                 table = "SURVIVAL.EXPAND_NO_MISSING1", driver = "OpalDriver")
  builder$append(server = "study2", 
                 url = "http://192.168.56.100:8080/", 
                 user = "administrator", password = "datashield_test&", 
                 table = "SURVIVAL.EXPAND_NO_MISSING2", driver = "OpalDriver")
  builder$append(server = "study3",
                 url = "http://192.168.56.100:8080/", 
                 user = "administrator", password = "datashield_test&", 
                 table = "SURVIVAL.EXPAND_NO_MISSING3", driver = "OpalDriver")
  logindata <- builder$build()
  
  # Log onto the remote Opal training servers
  connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D") 
  
  # Fit the GLM 
  
  # make sure that the outcome is numeric 
  ds.asNumeric(x.name = "D$cens",
               newobj = "EVENT",
               datasources = connections)
               
  # convert time id variable to a factor 
               
  ds.asFactor(input.var.name = "D$time.id",
              newobj = "TID",
              datasources = connections)
              
  # create in the server-side the log(survtime) variable
         
  ds.log(x = "D$survtime",
         newobj = "log.surv",
         datasources = connections)
  
  ds.glmSLMA(formula = EVENT ~ 1 + TID + female * age.60,
         dataName = "D",
         family = "poisson", 
         offset = "log.surv",
         weights = NULL,
         checks = FALSE,
         maxit = 20,
         datasources = connections)
         
  # Clear the Datashield R sessions and logout
  datashield.logout(connections) 
  
  # Example 2: run a logistic regression without interaction
  # For this example we are going to load another type of data  
  
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
  
  # Fit the logistic regression model

  mod <- ds.glmSLMA(formula = "DIS_DIAB~GENDER+PM_BMI_CONTINUOUS+LAB_HDL",
                dataName = "D",
                family = "binomial",
                datasources = connections)
                
  mod #visualize the results of the model

# Example 3: fit a standard Gaussian linear model with an interaction
# We are using the same data as in example 2. It is not necessary to
# connect again to the server 

mod <- ds.glmSLMA(formula = "PM_BMI_CONTINUOUS~DIS_DIAB*GENDER+LAB_HDL",
              dataName = "D",
              family = "gaussian",
              datasources = connections)
mod

# Clear the Datashield R sessions and logout
datashield.logout(connections) 
} # }
```
