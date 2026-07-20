# Sorts data frames in the server-side

Sorts a data frame using a specified sort key.

## Usage

``` r
ds.dataFrameSort(
  df.name = NULL,
  sort.key.name = NULL,
  sort.descending = FALSE,
  sort.method = "default",
  newobj = NULL,
  datasources = NULL
)
```

## Arguments

- df.name:

  a character string providing the name of the data frame to be sorted.

- sort.key.name:

  a character string providing the name for the sort key.

- sort.descending:

  logical, if TRUE the data frame will be sorted. by the sort key in
  descending order. Default = FALSE (sort order ascending).

- sort.method:

  a character string that specifies the method to be used to sort the
  data frame. This can be set as `"alphabetic"`,`"a"` or `"numeric"`,
  `"n"`.

- newobj:

  a character string that provides the name for the output data frame
  that is stored on the data servers. Default `dataframesort.newobj`.
  where `df.name` is the first argument of `ds.dataFrameSort()`.

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the `datasources` argument is not
  specified the default set of connections will be used: see
  [`datashield.connections_default`](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

## Value

`ds.dataFrameSort` returns the sorted data frame which is written to the
server-side.

## Details

It sorts a specified data.frame on the serverside using a sort key also
on the server-side. The sort key can either sit in the data.frame or
outside it. The sort key can be forced to be interpreted as alphabetic
or numeric.

When a numeric vector is sorted alphabetically, the order can look
confusing. For example, if we have a numeric vector to sort:  
vector.2.sort = c(-192, 76, 841, NA, 1670, 163, 147, 101, -112, -231,
-9, 119, 112, NA)  

When sorting numbers in an ascending (default) manner, the largest
negative numbers get ordered first leading up to the largest positive
numbers and finally (by default in R) NAs being positioned at the end of
the vector:  
numeric.sort = c(-231, -192, -112, -9, 76, 101, 112, 119, 147, 163, 841,
1670, NA, NA)  

Instead, if the same vector is sorted alphabetically the the resultant
vector is:

alphabetic.sort = (-112, -192, -231, -9, 101, 112, 119, 147, 163, 1670,
76, 841, NA, NA)  

Server function called: `dataFrameSortDS`.

## Author

DataSHIELD Development Team

Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands

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
  
  # Sorting the data frame
  ds.dataFrameSort(df.name = "D",
                   sort.key.name = "D$LAB_TSC",
                   sort.descending = TRUE,
                   sort.method = "numeric",
                   newobj = "df.sort",
                   datasources = connections[1]) #only the first Opal server is used ("study1")
                   
  # Clear the Datashield R sessions and logout                 
  datashield.logout(connections) 
  
} # }   
```
