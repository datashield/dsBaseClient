# Converts a server-side R object into Boolean indicators

It compares R objects using the standard set of Boolean operators
(`==, !=, >, >=, <, <=`) to create a vector with Boolean indicators that
can be of class logical (`TRUE/FALSE`) or numeric (`1/0`).

## Usage

``` r
ds.Boole(
  V1 = NULL,
  V2 = NULL,
  Boolean.operator = NULL,
  numeric.output = TRUE,
  na.assign = "NA",
  newobj = NULL,
  datasources = NULL
)
```

## Arguments

- V1:

  A character string specifying the name of the vector to which the
  Boolean operator is to be applied.

- V2:

  A character string specifying the name of the vector to compare with
  `V1`.

- Boolean.operator:

  A character string specifying one of six possible Boolean operators:
  `'==', '!=', '>', '>=', '<'` and `'<='`.

- numeric.output:

  logical. If TRUE the output variable should be of class numeric
  (`1/0`). If FALSE the output variable should be of class logical
  (`TRUE/FALSE`). Default TRUE.

- na.assign:

  A character string taking values `'NA'`,`'1'` or `'0'`. Default
  `'NA'`. For more information see details.

- newobj:

  a character string that provides the name for the output object that
  is stored on the data servers. Default `boole.newobj`.

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the `datasources` argument is not
  specified the default set of connections will be used: see
  [`datashield.connections_default`](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

## Value

`ds.Boole` returns the object specified by the `newobj` argument which
is written to the server-side. Also, two validity messages are returned
to the client-side indicating the name of the `newobj` which has been
created in each data source and if it is in a valid form.

## Details

A combination of different Boolean operators using `AND` operator can be
obtained by multiplying two or more binary/Boolean vectors together. In
this way, observations taking the value 1 in every vector will then take
the value 1 in the final vector (after multiplication) while all others
will take the value 0. Instead the combination using `OR` operator can
be obtained by the sum of two or more vectors and applying `ds.Boole`
using the operator `>= 1`.

In `na.assign` if `'NA'` is specified, the missing values remain as
`NA`s in the output vector. If `'1'` or `'0'` is specified the missing
values are converted to 1 or 0 respectively or `TRUE` or `FALSE`
depending on the argument `numeric.output`.

Server function called: `BooleDS`

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

  # Generating Boolean indicators
  ds.Boole(V1 = "D$LAB_TSC",
           V2 = "D$LAB_TRIG",
           Boolean.operator = ">",
           numeric.output = TRUE, #Output vector of 0 and 1
           na.assign = "NA",      
           newobj = "Boole.vec",
           datasources = connections[1]) #only the first server is used ("study1")
           
  ds.Boole(V1 = "D$LAB_TSC",
           V2 = "D$LAB_TRIG",
           Boolean.operator = "<",
           numeric.output = FALSE, #Output vector of TRUE and FALSE 
           na.assign = "1", #NA values are converted to TRUE
           newobj = "Boole.vec",
           datasources = connections[2]) #only the second server is used ("study2") 
                      
  ds.Boole(V1 = "D$LAB_TSC",
           V2 = "D$LAB_TRIG",
           Boolean.operator = ">",
           numeric.output = TRUE, #Output vector of 0 and 1
           na.assign = "0", #NA values are converted to 0      
           newobj = "Boole.vec",
           datasources = connections) #All servers are used
  
  # Clear the Datashield R sessions and logout           
  datashield.logout(connections)
} # }
 
```
