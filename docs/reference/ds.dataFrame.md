# Generates a data frame object in the server-side

Creates a data frame from its elemental components: pre-existing data
frames, single variables or matrices.

## Usage

``` r
ds.dataFrame(
  x = NULL,
  row.names = NULL,
  check.rows = FALSE,
  check.names = TRUE,
  stringsAsFactors = TRUE,
  completeCases = FALSE,
  DataSHIELD.checks = FALSE,
  newobj = NULL,
  datasources = NULL,
  notify.of.progress = FALSE,
  classConsistencyCheck = TRUE
)
```

## Arguments

- x:

  a character string that provides the name of the objects to be
  combined.

- row.names:

  NULL, integer or character string that provides the row names of the
  output data frame.

- check.rows:

  logical. If TRUE then the rows are checked for consistency of length
  and names. Default is FALSE.

- check.names:

  logical. If TRUE the column names in the data frame are checked to
  ensure that is unique. Default is TRUE.

- stringsAsFactors:

  logical. If true the character vectors are converted to factors.
  Default TRUE.

- completeCases:

  logical. If TRUE rows with one or more missing values will be deleted
  from the output data frame. Default is FALSE.

- DataSHIELD.checks:

  logical. Default FALSE. If TRUE undertakes all DataSHIELD checks
  (time-consuming) which are:  
  1. the input object(s) is(are) defined in all the studies  
  2. the input object(s) is(are) of the same legal class in all the
  studies  
  3. if there are any duplicated column names in the input objects in
  each study  
  4. the number of rows of the data frames or matrices and the length of
  all component variables are the same

- newobj:

  a character string that provides the name for the output data frame
  that is stored on the data servers. Default `dataframe.newobj`.

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the `datasources` argument is not
  specified the default set of connections will be used: see
  [`datashield.connections_default`](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

- notify.of.progress:

  specifies if console output should be produced to indicate progress.
  Default is FALSE.

- classConsistencyCheck:

  logical. If TRUE, verifies that each input object has the same class
  across all studies. Default TRUE.

## Value

`ds.dataFrame` returns the object specified by the `newobj` argument
which is written to the server-side.

## Details

It creates a data frame by combining pre-existing data frames, matrices
or variables.

The length of all component variables and the number of rows of the data
frames or matrices must be the same. The output data frame will have the
same number of rows.

Server functions called: `classDS`, `colnamesDS`, `dataFrameDS`

## Author

DataSHIELD Development Team

Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands

## Examples

``` r
if (FALSE) { # \dontrun{

  ## Version 6, for version 5 see the Wiki 
  # Connecting to the Opal servers

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
  
  # Log onto the remote Opal training servers
  connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D") 
  
  # Create a new data frame
  ds.dataFrame(x = c("D$LAB_TSC","D$GENDER","D$PM_BMI_CATEGORICAL"),
               row.names = NULL,
               check.rows = FALSE,
               check.names = TRUE,
               stringsAsFactors = TRUE, #character variables are converted to a factor 
               completeCases = TRUE, #only rows with not missing values are selected
               DataSHIELD.checks = FALSE,
               newobj = "df1",
               datasources = connections[1], #only the first Opal server is used ("study1")
               notify.of.progress = FALSE)


  # Clear the Datashield R sessions and logout
  datashield.logout(connections) 
} # }
```
