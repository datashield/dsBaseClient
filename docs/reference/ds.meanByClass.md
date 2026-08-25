# Computes the mean and standard deviation across categories

This function calculates the mean and the standard deviation (SD) of a
continuous variable for each class of up to 3 categorical variables.

## Usage

``` r
ds.meanByClass(
  x = NULL,
  outvar = NULL,
  covar = NULL,
  type = "combine",
  datasources = NULL
)
```

## Arguments

- x:

  a character string specifying the name of the dataset or a text
  formula.

- outvar:

  a character vector specifying the names of the continuous variables.

- covar:

  a character vector specifying the names of up to 3 categorical
  variables

- type:

  a character string that represents the type of analysis to carry out.
  `type` can be set as: `'combine'` or `'split'`. Default `'combine'`.
  For more information see **Details**.

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the `datasources` argument is not
  specified the default set of connections will be used: see
  [`datashield.connections_default`](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

## Value

`ds.meanByClass` returns to the client-side a table or a list of tables
that hold the length of the numeric variable(s) and their mean and
standard deviation in each subgroup (subset).

## Details

The function splits the input dataset into subsets (one for each
category) and calculates the mean and SD of the specified numeric
variables. It is important to note that the process of generating the
final table(s) can be time consuming particularly if the subsetting is
done across more than one categorical variable and the run-time
lengthens if the parameter `type` is set to `'split'` as a table is then
produced for each study. It is therefore advisable to run the function
only for the studies of the user interested in but including only those
studies in the parameter `datasources`.

Depending on the variable `type` can be carried out two analysis:  
(1) `'combine'`: a pooled table of results is generated.  
(2) `'split'`: a table of results is generated for each study.

## Author

DataSHIELD Development Team

## Examples

``` r
if (FALSE) { # \dontrun{

 ## Version 6, for version 5 see the Wiki
  
  # connecting to the Opal servers

  require('DSI')
  require('DSOpal')
  require('dsBaseClient')

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
  
  
  connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D") 
  
  #Calculate mean by class
  
  ds.meanByClass(x = "D",
                 outvar = c('LAB_HDL','LAB_TSC'),
                 covar = c('PM_BMI_CATEGORICAL'),
                 type = "combine",
                 datasources = connections)
                 
  ds.meanByClass(x = "D$LAB_HDL~D$PM_BMI_CATEGORICAL",
                 type = "combine",
                 datasources = connections[1])#Only the frist server is used ("study1")  
             
  # clear the Datashield R sessions and logout
  datashield.logout(connections)
} # }
```
