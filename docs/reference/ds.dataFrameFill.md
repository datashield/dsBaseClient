# Creates missing values columns in the server-side

Adds extra columns with missing values in a data frame on the
server-side.

## Usage

``` r
ds.dataFrameFill(
  df.name = NULL,
  newobj = NULL,
  datasources = NULL,
  classConsistencyCheck = TRUE
)
```

## Arguments

- df.name:

  a character string representing the name of the input data frame that
  will be filled with extra columns of missing values.

- newobj:

  a character string that provides the name for the output data frame
  that is stored on the data servers. Default value is
  "dataframefill.newobj".

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the `datasources` argument is not
  specified the default set of connections will be used: see
  [`datashield.connections_default`](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

- classConsistencyCheck:

  logical. If TRUE, verifies that each input object has the same class
  across all studies. Default TRUE.

## Value

`ds.dataFrameFill` returns the object specified by the `newobj` argument
which is written to the server-side.

## Details

This function checks if the input data frames have the same variables
(i.e. the same column names) in all of the used studies. When a study
does not have some of the variables, the function generates those
variables as vectors of missing values and combines them as columns to
the input data frame. If any of the generated variables are of class
factor, the function assigns to those the corresponding levels of the
factors given from the studies where such factors exist.

Server function called: `dataFrameFillDS`

## Author

Demetris Avraam for DataSHIELD Development Team

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
  
  # Create two data frames with one different column
  
  ds.dataFrame(x = c("D$LAB_TSC","D$LAB_TRIG","D$LAB_HDL",
                     "D$LAB_GLUC_ADJUSTED","D$PM_BMI_CONTINUOUS"),
               newobj = "df1",
               datasources = connections[1])
               
  ds.dataFrame(x = c("D$LAB_TSC","D$LAB_TRIG","D$LAB_HDL","D$LAB_GLUC_ADJUSTED"),
               newobj = "df1",
               datasources = connections[2])
  
  # Fill the data frame with NA columns
  
  ds.dataFrameFill(df.name = "df1",
                   newobj = "D.Fill",
                   datasources = connections[c(1,2)]) # Two servers are used


  # Clear the Datashield R sessions and logout
  datashield.logout(connections) 
} # }
```
