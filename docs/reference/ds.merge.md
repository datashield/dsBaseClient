# Merges two data frames in the server-side

Merges (links) two data frames together based on common values in
defined vectors in each data frame.

## Usage

``` r
ds.merge(
  x.name = NULL,
  y.name = NULL,
  by.x.names = NULL,
  by.y.names = NULL,
  all.x = FALSE,
  all.y = FALSE,
  sort = TRUE,
  suffixes = c(".x", ".y"),
  no.dups = TRUE,
  incomparables = NULL,
  newobj = NULL,
  datasources = NULL
)
```

## Arguments

- x.name:

  a character string specifying the name of the first data frame to be
  merged. The length of the string should be less than the specified
  threshold for the nfilter.stringShort which is one of the disclosure
  prevention checks in DataSHIELD.

- y.name:

  a character string specifying the name of the second data frame to be
  merged. The length of the string should be less than the specified
  threshold for the nfilter.stringShort which is one of the disclosure
  prevention checks in DataSHIELD.

- by.x.names:

  a character string or a vector of names specifying of the column(s) in
  data frame `x.name` for merging.

- by.y.names:

  a character string or a vector of names specifying of the column(s) in
  data frame `y.name` for merging.

- all.x:

  logical. If TRUE then extra rows will be added to the output, one for
  each row in `x.name` that has no matching row in `y.name`. If FALSE
  the rows with data from both data frames are included in the output.
  Default FALSE.

- all.y:

  logical. If TRUE then extra rows will be added to the output, one for
  each row in `y.name` that has no matching row in `x.name`. If FALSE
  the rows with data from both data frames are included in the output.
  Default FALSE.

- sort:

  logical. If TRUE the merged result is sorted on elements in the
  `by.x.names` and `by.y.names` columns. Default TRUE.

- suffixes:

  a character vector of length 2 specifying the suffixes to be used for
  making unique common column names in the two input data frames when
  they both appear in the merged data frame.

- no.dups:

  logical. Suffixes are appended in more cases to avoid duplicated
  column names in the merged data frame. Default TRUE (FALSE before R
  version 3.5.0).

- incomparables:

  values that cannot be matched. This is intended to be used for merging
  on one column, so these are incomparable values of that column. For
  more information see `match` in native R `merge` function.

- newobj:

  a character string that provides the name for the output variable that
  is stored on the data servers. Default `merge.newobj`.

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the `datasources` argument is not
  specified the default set of connections will be used: see
  [`datashield.connections_default`](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

## Value

`ds.merge` returns the merged data frame that is written on the
server-side. Also, two validity messages are returned to the client-side
indicating whether the new object has been created in each data source
and if so whether it is in a valid form.

## Details

This function is similar to the native R function `merge`. There are
some changes compared with the native R function in choosing which
variables to use to merge the data frames, the function `merge` is very
flexible. For example, you can choose to merge using all vectors that
appear in both data frames. However, for `ds.merge` in DataSHIELD it is
required that all the vectors which dictate the merging are explicitly
identified for both data frames using the `by.x.names` and `by.y.names`
arguments.

Server function called: `mergeDS`

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
  
  #Create two data frames with a common column
  
  ds.dataFrame(x = c("D$LAB_TSC","D$LAB_TRIG","D$LAB_HDL","D$LAB_GLUC_ADJUSTED"),
               completeCases = TRUE,
               newobj = "df.x",
               datasources = connections)
               
  ds.dataFrame(x = c("D$LAB_TSC","D$GENDER","D$PM_BMI_CATEGORICAL","D$PM_BMI_CONTINUOUS"),
               completeCases = TRUE,
               newobj = "df.y",
               datasources = connections) 
               
  # Merge data frames using the common variable "LAB_TSC"
               
  ds.merge(x.name = "df.x",
           y.name = "df.y",
           by.x.names = "df.x$LAB_TSC",
           by.y.names = "df.y$LAB_TSC",
           all.x = TRUE,
           all.y = TRUE,
           sort = TRUE,
           suffixes = c(".x", ".y"),
           no.dups = TRUE,
           newobj = "df.merge",
           datasources = connections)              
  
  # clear the Datashield R sessions and logout
  datashield.logout(connections)
} # }
```
