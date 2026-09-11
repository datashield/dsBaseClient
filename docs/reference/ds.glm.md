# Fits Generalized Linear Model

Fits a Generalized Linear Model (GLM) on data from single or multiple
sources on the server-side.

## Usage

``` r
ds.glm(
  formula = NULL,
  data = NULL,
  family = NULL,
  offset = NULL,
  weights = NULL,
  checks = FALSE,
  maxit = 20,
  CI = 0.95,
  viewIter = FALSE,
  viewVarCov = FALSE,
  viewCor = FALSE,
  datasources = NULL
)
```

## Arguments

- formula:

  an object of class formula describing the model to be fitted. For more
  information see **Details**.

- data:

  a character string specifying the name of an (optional) data frame
  that contains all of the variables in the GLM formula.

- family:

  identifies the error distribution function to use in the model. This
  can be set as `"gaussian"`, `"binomial"` and `"poisson"`. For more
  information see **Details**.

- offset:

  a character string specifying the name of a variable to be used as an
  offset. `ds.glm` does not allow an offset vector to be written
  directly into the GLM formula. For more information see **Details**.

- weights:

  a character string specifying the name of a variable containing prior
  regression weights for the fitting process. `ds.glm` does not allow a
  weights vector to be written directly into the GLM formula.

- checks:

  logical. If TRUE `ds.glm` checks the structural integrity of the
  model. Default FALSE. For more information see **Details**.

- maxit:

  a numeric scalar denoting the maximum number of iterations that are
  permitted before `ds.glm` declares that the model has failed to
  converge.

- CI:

  a numeric value specifying the confidence interval. Default `0.95`.

- viewIter:

  logical. If TRUE the results of the intermediate iterations are
  printed. If FALSE only final results are shown. Default FALSE.

- viewVarCov:

  logical. If TRUE the variance-covariance matrix of parameter estimates
  is returned. Default FALSE.

- viewCor:

  logical. If TRUE the correlation matrix of parameter estimates is
  returned. Default FALSE.

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the `datasources` argument is not
  specified the default set of connections will be used: see
  [`datashield.connections_default`](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

## Value

Many of the elements of the output list returned by `ds.glm` are
equivalent to those returned by the
[`glm()`](https://rdrr.io/r/stats/glm.html) function in native R.
However, potentially disclosive elements such as individual-level
residuals and linear predictor values are blocked. In this case, only
non-disclosive elements are returned from each study separately.

The list of elements returned by `ds.glm` is mentioned below:

`Nvalid`: total number of valid observational units across all studies.

`Nmissing`: total number of observational units across all studies with
at least one data item missing.

`Ntotal`: total of observational units across all studies, the sum of
valid and missing units.

`disclosure.risk`: risk of disclosure, the value 1 indicates that one of
the disclosure traps has been triggered in that study.

`errorMessage`: explanation for any errors or disclosure risks
identified.

`nsubs`: total number of observational units used by `ds.glm` function.
`nb` usually is the same as `nvalid`.

`iter`: total number of iterations before convergence achieved.

`family`: error family and link function.

`formula`: model formula, see description of formula as an input
parameter (above).

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

`dev`: residual deviance.

`df`: residual degrees of freedom. `nb` residual degrees of freedom +
number of parameters in model = `nsubs`.

`output.information`: reminder to the user that there is more
information at the top of the output.

Also, the estimated coefficients and standard errors expanded with
estimated confidence intervals with % coverage specified by `ci`
argument are returned. For the poisson model, the output is generated on
the scale of the linear predictor (log rates and log rate ratios) and
the natural scale after exponentiation (rates and rate ratios).

## Details

Fits a GLM on data from a single source or multiple sources on the
server-side. In the latter case, the data are co-analysed (when using
`ds.glm`) by using an approach that is mathematically equivalent to
placing all individual-level data from all sources in one central
warehouse and analysing those data using the conventional
[`glm()`](https://rdrr.io/r/stats/glm.html) function in R. In this
situation marked heterogeneity between sources should be corrected
(where possible) with fixed effects. For example, if each study in a
(binary) logistic regression analysis has an independent intercept, it
is equivalent to allowing each study to have a different baseline risk
of disease. This may also be viewed as being an IP (individual person)
meta-analysis with fixed effects.

In `formula` most shortcut notation for formulas allowed under R's
standard [`glm()`](https://rdrr.io/r/stats/glm.html) function is also
allowed by `ds.glm`.

Many GLMs can be fitted very simply using a formula such as:

\\y~a+b+c+d\\

which simply means fit a GLM with `y` as the outcome variable and `a`,
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

In the `family` argument can be specified three types of models to fit:

- `"gaussian"`:

  : conventional linear model with normally distributed errors

- `"binomial"`:

  : conventional unconditional logistic regression model

- `"poisson"`:

  : Poisson regression model which is the most used in survival
  analysis. The model used Piecewise Exponential Regression (PER) which
  typically closely approximates Cox regression in its main estimates
  and standard errors.

At present the gaussian family is automatically coupled with an
`identity` link function, the binomial family with a `logistic` link
function and the poisson family with a `log` link function.

The `data` argument avoids you having to specify the name of the data
frame in front of each covariate in the formula. For example, if the
data frame is called `DataFrame` you avoid having to write:
\\DataFrame\\y ~ DataFrame\\a + DataFrame\\b + DataFrame\\c +
DataFrame\\d\\

The `checks` argument verifies that the variables in the model are all
defined (exist) on the server-side at every study and that they have the
correct characteristics required to fit the model. It is suggested to
make `checks` argument TRUE if an unexplained problem in the model fit
is encountered because the running process takes several minutes.

In `maxit` Logistic regression and Poisson regression models can require
many iterations, particularly if the starting value of the regression
constant is far away from its actual value that the GLM is trying to
estimate. In consequence we often set `maxit=30` but depending on the
nature of the models you wish to fit, you may wish to be alerted much
more quickly than this if there is a delay in convergence, or you may
wish to all more iterations.

Privacy protected iterative fitting of a GLM is explained here:

\(1\) Begin with a guess for the coefficient vector to start iteration 1
(let's call it `beta.vector[1]`). Using `beta.vector[1]`, run iteration
1 with each source calculating the resultant score vector (and
information matrix) generated by its data - given `beta.vector[1]` - as
the sum of the score vector components (and the sum of the components of
the information matrix) derived from each individual data record in that
source. NB in most models the starting values in `beta.vector[1]` are
set to be zero for all parameters.

\(2\) Transmit the resultant score vector and information matrix from
each source back to the clientside server (CS) at the analysis centre.
Let's denote `SCORE[1][j]` and `INFORMATION.MATRIX[1][j]` as the score
vector and information matrix generated by study `j` at the end of the
1st iteration.

\(3\) CS sums the score vectors, and equivalently the information
matrices, across all studies (i.e. `j = 1:S`, where `S` is the number of
studies). Note that, given `beta.vector[1]`, this gives precisely the
same final sums for the score vectors and information matrices as would
have been obtained if all data had been in one central warehoused
database and the overall score vector and information matrix at the end
of the first iteration had been calculated (as is standard) by simply
summing across all individuals. The only difference is that instead of
directly adding all values across all individuals, we first sum across
all individuals in each data source and then sum those study totals
across all studies - i.e. this generates the same ultimate sums

\(4\) CS then calculates
`sum(SCORES)%*% inverse(sum(INFORMATION.MATRICES))` - heuristically this
may be viewed as being "the sum of the score vectors divided (NB 'matrix
division') by the sum of the information matrices". If one uses the
conventional algorithm (IRLS) to update generalized linear models from
iteration to iteration this quantity happens to be precisely the vector
to be added to the current value of beta.vector (i.e. `beta.vector[1]`)
to obtain `beta.vector[2]` which is the improved estimate of the
beta.vector to be used in iteration 2. This updating algorithm is often
called the IRLS (Iterative Reweighted Least Squares) algorithm - which
is closely related to the Newton Raphson approach but uses the expected
information rather than the observed information.

\(5\) Repeat steps (2)-(4) until the model converges (using the standard
R convergence criterion). NB An alternative way to coherently pool the
glm across multiple sources is to fit each glm to completion (i.e.
multiple iterations until convergence) in each source and then return
the final parameter estimates and standard errors to the CS where they
could be pooled using study-level meta-analysis. An alternative function
ds.glmSLMA allows you to do this. It will fit the glms to completion in
each source and return the final estimates and standard errors (rather
than score vectors and information matrices). It will then rely on
functions in the R package metafor to meta-analyse the key parameters.

Server functions called: `glmDS1` and `glmDS2`

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
  
  ds.glm(formula = EVENT ~ 1 + TID + female * age.60,
         data = "D",
         family = "poisson", 
         offset = "log.surv",
         weights = NULL,
         checks = FALSE,
         maxit = 20,
         CI = 0.95,
         viewIter = FALSE,
         viewVarCov = FALSE,
         viewCor = FALSE,
         datasources = connections)
         
  # Clear the Datashield R sessions and logout
  datashield.logout(connections) 
  
  # Example 2: run a logistic regression without interaction
  # For this example we are going to load another dataset  
  
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

  mod <- ds.glm(formula = "DIS_DIAB~GENDER+PM_BMI_CONTINUOUS+LAB_HDL",
                data = "D",
                family = "binomial",
                datasources = connections)
                
  mod #visualize the results of the model

# Example 3: fit a standard Gaussian linear model with an interaction
# We are using the same data as in example 2. 

mod <- ds.glm(formula = "PM_BMI_CONTINUOUS~DIS_DIAB*GENDER+LAB_HDL",
              data = "D",
              family = "gaussian",
              datasources = connections)
mod

# Clear the Datashield R sessions and logout
datashield.logout(connections) 
} # }
```
