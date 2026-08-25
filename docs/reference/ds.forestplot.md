# Forestplot for SLMA models

Draws a foresplot of the coefficients for Study-Level Meta-Analysis
performed with DataSHIELD

## Usage

``` r
ds.forestplot(mod, variable = NULL, method = "ML", layout = "JAMA")
```

## Arguments

- mod:

  `list` List outputed by any of the SLMA models of DataSHIELD
  (`ds.glmerSLMA`, `ds.glmSLMA`, `ds.lmerSLMA`)

- variable:

  `character` (default `NULL`) Variable to meta-analyze and visualize,
  by setting this argument to `NULL` (default) the first independent
  variable will be used.

- method:

  `character` (Default `"ML"`) Method to estimate the between study
  variance. See details from
  [`?meta::metagen`](https://rdrr.io/pkg/meta/man/metagen.html) for the
  different options.

- layout:

  `character` (default `"JAMA"`) Layout of the plot. See details from
  [`?meta::metagen`](https://rdrr.io/pkg/meta/man/metagen.html) for the
  different options.

## Examples

``` r
if (FALSE) { # \dontrun{
  # Run a logistic regression
  
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
                data = "D",
                family = "binomial",
                datasources = connections)
                
  # Plot the results of the model
  ds.forestplot(mod)
} # }
```
