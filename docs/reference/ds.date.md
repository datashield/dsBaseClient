# Creates date objects using a server-side object

Generates objects using a server-side object, which can be either a
vector or a data-frame column. Supports three operations: 1. Extract
components of a date (`extractdate`) 2. Combine numeric year, month, and
day into a full date (`makedate`) 3. Compute the time interval between
two dates (`timebetween`)

## Usage

``` r
ds.date(
  x = NULL,
  type = c("extractdate", "makedate", "timebetween"),
  unit = c("days", "months", "years"),
  add.column = FALSE,
  newobj = "date.result",
  datasources = NULL
)
```

## Arguments

- x:

  Character vector specifying the server-side object(s). For data-frame
  columns, use the format `df$column`.

- type:

  Character string specifying the operation: `"extractdate"`,
  `"makedate"`, or `"timebetween"`.

- unit:

  Character string specifying the unit for `extractdate` or
  `timebetween`: `"days"`, `"months"`, or `"years"`.

- add.column:

  Logical. If `FALSE`, the result is created as a new server-side
  object; if `TRUE`, the result is added as a new column in the existing
  data-frame. Default is `FALSE`.

- newobj:

  Character string for the name of the object that will be created on
  the server. Default is `"date.result"`.

- datasources:

  A list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the `datasources` argument is not
  specified the default set of connections will be used: see
  [`datashield.connections_default`](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

## Details

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
  connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D")

  ds.make(toAssign = "D$date_diagnosis",
          newobj = 'diagnosis_date', datasources = connections)
  ds.date(x="D$date_diagnosis", type = "extractdate",
        newobj = "diag_month", unit = "months", add.column = TRUE)
  ds.date(x="D$date_diagnosis", type = "extractdate",
          newobj = "diag_day", unit = "days", add.column = TRUE)


  # Example 1: Create a new object by extracting the year from an object
  ds.date(x="diagnosis_date", type = "extractdate",
          newobj = "diagnosis_year", unit = "years", add.column = FALSE)

  # Example 2: Create a new column by extracting year from an object. This will result in 
  # an error since creating a new column option requires a dataframe input.
  ds.date(x="diagnosis_date", type = "extractdate",
          newobj = "diagnosis_year", unit = "years", add.column = TRUE)

  # Example 3: Create a new date column by combining 3 objects: 2 columns and 1 vector.
  ds.date(x=c("diagnosis_year", "D$diag_month", "D$diag_day"), type = "makedate",
          newobj = "combined_date", add.column = TRUE)

  # Example 4: Create a new object by calculating time between one column and one
  # object in months.
  ds.date(x=c("diagnosis_date", "D$date_death"), type = "timebetween",
          newobj = "timebetween.months", unit = "months", add.column = FALSE)

  # Clear the Datashield R sessions and logout
  datashield.logout(connections)
} # }
 
```
