# Changes the reference level of a factor in the server-side

Change the reference level of a factor, by putting the reference group
first.

This function is similar to R function `relevel`.

## Usage

``` r
ds.changeRefGroup(
  x = NULL,
  ref = NULL,
  newobj = NULL,
  reorderByRef = FALSE,
  datasources = NULL
)
```

## Arguments

- x:

  a character string providing the name of the input vector of type
  factor.

- ref:

  the reference level.

- newobj:

  a character string that provides the name for the output object that
  is stored on the server-side. Default `changerefgroup.newobj`.

- reorderByRef:

  logical, if TRUE the new vector should be ordered by the reference
  group (i.e. putting the reference group first). The default is to not
  re-order (see the reasons in the details).

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the `datasources` argument is not
  specified the default set of connections will be used: see
  [`datashield.connections_default`](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

## Value

`ds.changeRefGroup` returns a new vector with the specified level as a
reference which is written to the server-side.

## Details

This function allows the user to re-order the vector, putting the
reference group first. It should be mentioned that by default the
reference is the first level in the vector of levels. If the user
chooses the re-order a warning is issued as this can introduce a
mismatch of values if the vector is put back into a table that is not
reordered in the same way. Such mismatch can render the results of
operations on that table invalid.

Server function called: `changeRefGroupDS`

## See also

[`ds.cbind`](ds.cbind.md) Combines objects column-wise.

[`ds.levels`](ds.levels.md) to obtain the levels (categories) of a
vector of type factor.

[`ds.colnames`](ds.colnames.md) to obtain the column names of a matrix
or a data frame

[`ds.asMatrix`](ds.asMatrix.md) to coerce an object into a matrix type.

[`ds.dim`](ds.dim.md) to obtain the dimensions of a matrix or a data
frame.

## Author

DataSHIELD Development Team

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

  # Changing the reference group in the server-side
 
    # Example 1: rename the categories and change the reference with re-ordering
      # print out the levels of the initial vector
      ds.levels(x= "D$PM_BMI_CATEGORICAL",
                datasources = connections)

      # define a vector with the new levels and recode the initial levels
      newNames <- c("normal", "overweight", "obesity")
      ds.recodeLevels(x = "D$PM_BMI_CATEGORICAL",
                      newCategories = newNames,
                      newobj = "bmi_new",
                      datasources = connections)

      # print out the levels of the new vector
      ds.levels(x = "bmi_new",
                datasources = connections)

      # Set the reference to "obesity" without changing the order (default)
      ds.changeRefGroup(x = "bmi_new",
                        ref = "obesity",
                        newobj = "bmi_ob",
                        datasources = connections)

      # print out the levels; the first listed level (i.e. the reference) is now 'obesity'
      ds.levels(x = "bmi_ob",
                datasources = connections)

    # Example 2: change the reference and re-order by the reference level
      # If re-ordering is sought, the action is completed but a warning is issued
      ds.recodeLevels(x = "D$PM_BMI_CATEGORICAL",
                      newCategories = newNames,
                      newobj = "bmi_new",
                     datasources = connections)
      ds.changeRefGroup(x = "bmi_new",
                        ref = "obesity",
                        newobj = "bmi_ob",
                        reorderByRef = TRUE,
                        datasources = connections)

           
  # Clear the Datashield R sessions and logout
  datashield.logout(connections) 
} # }
```
