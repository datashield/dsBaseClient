# Generates 1-dimensional contingency tables

The function ds.table1D is a client-side wrapper function. It calls the
server-side function table1DDS to generate 1-dimensional tables for all
data sources.

## Usage

``` r
ds.table1D(
  x = NULL,
  type = "combine",
  warningMessage = TRUE,
  datasources = NULL
)
```

## Arguments

- x:

  a character, the name of a numerical vector with discrete values -
  usually a factor.

- type:

  a character which represent the type of table to output: pooled table
  or one table for each data source. If `type` is set to 'combine', a
  pooled 1-dimensional table is returned; if If `type` is set to 'split'
  a 1-dimensional table is returned for each data source.

- warningMessage:

  a boolean, if set to TRUE (default) a warning is displayed if any
  returned table is invalid. Warning messages are suppressed if this
  parameter is set to FALSE. However the analyst can still view
  'validity' information which are stored in the output object
  'validity' - see the list of output objects.

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the \<datasources\> the default set
  of connections will be used: see
  [datashield.connections_default](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

## Value

A list object containing the following items:

- counts:

  table(s) that hold counts for each level/category. If some cells
  counts are invalid (see 'Details' section) only the total (outer) cell
  counts are displayed in the returned individual study tables or in the
  pooled table.

- percentages:

  table(s) that hold percentages for each level/category. Here also
  inner cells are reported as missing if one or more cells are
  'invalid'.

- validity:

  a text that informs the analyst about the validity of the output
  tables. If any tables are invalid the studies they are originated from
  are also mentioned in the text message.

## Details

The table returned by the server side function might be valid (non
disclosive - no table cell have counts between 1 and the minimal number
agreed by the data owner and set in the data repository) or invalid
(potentially disclosive - one or more table cells have a count between 1
and the minimal number agreed by the data owner). If a 1-dimensional
table is invalid all the cells are set to NA except the total count.
This way it is possible the know the total count and combine total
counts across data sources but it is not possible to identify the
cell(s) that had the small counts which render the table invalid.

## See also

[ds.table2D](ds.table2D.md) for cross-tabulating two vectors.

## Author

Gaye, A.; Burton, P.

## Examples

``` r
if (FALSE) { # \dontrun{

  # load the file that contains the login details
  data(logindata)

  # login and assign all the stored variables to R
  conns <- datashield.login(logins=logindata,assign=TRUE)

  # Example 1: generate a one dimensional table, outputting combined (pooled) contingency tables
  output <- ds.table1D(x='D$GENDER')
  output$counts
  output$percentages
  output$validity

  # Example 2: generate a one dimensional table, outputting study specific contingency tables
  output <- ds.table1D(x='D$GENDER', type='split')
  output$counts
  output$percentages
  output$validity

  # Example 3: generate a one dimensional table, outputting study specific and combined
  # contingency tables - see what happens if the reruened table is 'invalid'.
  output <- ds.table1D(x='D$DIS_CVA')
  output$counts
  output$percentages
  output$validity

  # clear the Datashield R sessions and logout
  datashield.logout(conns)

} # }
```
