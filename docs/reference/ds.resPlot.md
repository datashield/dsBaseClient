# ds.resplots

Creates residual plots from `ds.glm`, using `ds.predict` and
`ds.scatterPlot`.

## Usage

``` r
ds.resPlot(
  name,
  traindataname,
  which = 0,
  pch = 1,
  col = "black",
  lty = 1,
  datasources = NULL
)
```

## Arguments

- name:

  The client-side return object from `ds.glm`.

- traindataname:

  The name of the dataset used to train the model.

- which:

  A numeric value deciding what type of plot to return. 1 = residuals vs
  fitted plot, 2 = QQ plot, 0 = both (default).

- datasources:

  A list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the `datasources` argument is not
  specified the default set of connections will be used: see
  [`datashield.connections_default`](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

## Details

This function generates residual plots (residuals vs fitted) and/or
QQ-plots for a model fitted with ds.glm, based on the \`which\`
argument.

## Author

Zulal Bekerecioglu

## Examples

``` r
if (FALSE) { # \dontrun{

 require('DSI')
 require('DSOpal')
 require('dsBaseClient')

 builder <- DSI::newDSLoginBuilder()
 builder$append(server = "study1", url = "https://opal-demo.obiba.org",
                user = "dsuser", password = "P@ssw0rd",
                table = "CNSIM.CNSIM1", driver = "OpalDriver")
 builder$append(server = "study2", url = "https://opal-demo.obiba.org",
                user = "dsuser", password = "P@ssw0rd",
                table = "CNSIM.CNSIM2", driver = "OpalDriver")
 logindata <- builder$build()

 # Log onto the remote Opal training servers
 connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D")


 # Example 1: Fit the model using ds.glm for study1
 fitted_model <- ds.glm(formula = "LAB_TSC ~ LAB_HDL + PM_BMI_CONTINUOUS * GENDER + MEDI_LPD",
                        data = "D", family = "gaussian", datasources = connections)

 # Residuals for the model
 ds.resPlot(name = fitted_model, traindataname="D", datasources = connections)


 # Clear the Datashield R sessions and logout
 datashield.logout(connections)
} # }
 
```
