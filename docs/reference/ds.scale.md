# Standardizes a server-side vector

Scales / standardizes a server-side vector using the scale function

## Usage

``` r
ds.scale(
  x = NULL,
  newobj = "scaled.data",
  add.column = FALSE,
  datasources = NULL
)
```

## Arguments

- x:

  A character string specifying the server-side vector For data-frame
  columns, use the format `df$column`.

- newobj:

  A character string for the name of the object that will be created on
  the server. Default is `"scaled.data"`.

- add.column:

  Logical. If `FALSE`, the result is created as a new server-side
  object; if `TRUE`, the result is added as a new column in the existing
  data-frame. Default is `FALSE`.

- datasources:

  A list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the `datasources` argument is not
  specified the default set of connections will be used: see
  [`datashield.connections_default`](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

## Details

Note: `add.column = TRUE` is only valid for data-frame inputs.

Server function called: `scaleDS`

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
                 table = "GWAS.ega_phenotypes_1", driver = "OpalDriver")
  builder$append(server = "study2",
                 url = "https://opal-demo.obiba.org",
                 user = "dsuser", password = "P@ssw0rd",
                 table = "GWAS.ega_phenotypes_2", driver = "OpalDriver")
  logindata <- builder$build()

  # Log onto the remote Opal training servers
  connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D")

  ds.make(toAssign = "D$energy", newobj = 'energy.obj', datasources = connections)

  # Example 1: Give a column, save as a new column
  ds.scale(x="D$age_recruitment", newobj="scaled.age.recruitment", add.column=TRUE)

  # Example 2: Give a numeric object, save as a new object
  ds.scale(x="energy.obj", newobj="scaled.energy", add.column=FALSE)
  
  
  # Clear the Datashield R sessions and logout           
  datashield.logout(connections)
} # }
 
```
