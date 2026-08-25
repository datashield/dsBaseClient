# ds.round

Generates objects using a server-side object, which can be either a
vector or a data-frame column. Supports five operations: 1. (`round`) 2.
(`ceiling`) 3. (`floor`) 4. (`trunc`) 5. (`signif`) where each function
in baseR is applied on the server side to the specified object.

## Usage

``` r
ds.round(
  x = NULL,
  type = c("round", "ceiling", "floor", "trunc", "signif"),
  digits = 0,
  add.column = FALSE,
  newobj = "rounding.result",
  datasources = NULL
)
```

## Arguments

- x:

  Character vector specifying the server-side object(s). For data-frame
  columns, use the format `df$column`.

- type:

  Character string specifying the operation: `"round"`, `"ceiling"`,
  `"floor"`, `trunc`, or `"signif"`.

- digits:

  Number of digits to be used in arguments `"round"` and `"signif"`.

- add.column:

  Logical. If `FALSE`, the result is created as a new server-side
  object; if `TRUE`, the result is added as a new column in the existing
  data-frame. Default is `FALSE`.

- newobj:

  Character string for the name of the object that will be created on
  the server. Default is `"rounding.result"`.

- datasources:

  A list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the `datasources` argument is not
  specified the default set of connections will be used: see
  [`datashield.connections_default`](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

## Details

Note: `add.column = TRUE` is only valid for data-frame inputs.

Server function called: `DateDS`

## Author

Zulal Bekerecioglu

## Examples

``` r
if (FALSE) { # \dontrun{

  require('DSI')
  require('DSOpal')
  require('dsBaseClient')

  builder <- DSI::newDSLoginBuilder()
  builder$append(server = "study1", 
                 url = "https://opal-demo.obiba.org", 
                 user = "dsuser", password = "P@ssw0rd", 
                 table = "DASIM.DASIM1", driver = "OpalDriver")
  builder$append(server = "study2", 
                 url = "https://opal-demo.obiba.org", 
                 user = "dsuser", password = "P@ssw0rd", 
                 table = "DASIM.DASIM2", driver = "OpalDriver")
  logindata <- builder$build()
  
  # Log onto the remote Opal training servers
  connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D") 
  
  ds.make(toAssign = "D$LAB_TSC", newobj = 'LAB.TSC.obj', datasources = connections)
  
  # Example 1: Give a numeric object, save as a new object
  ds.round("LAB.TSC.obj", digits=2, add.column = FALSE)
           
  # Example 2: Give a column, save as a new column.
  ds.round("D$LAB_HDL", type = "ceiling", newobj = "LAB_rounded_HDL", add.column = TRUE)
  
  
  # Clear the Datashield R sessions and logout           
  datashield.logout(connections)
} # }
 
```
