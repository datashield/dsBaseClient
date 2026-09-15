# Generates 2-dimensional contingency tables

The function ds.table2D is a client-side wrapper function. It calls the
server-side function 'table2DDS' that generates a 2-dimensional
contingency table for each data source.

## Usage

``` r
ds.table2D(
  x = NULL,
  y = NULL,
  type = "both",
  warningMessage = TRUE,
  datasources = NULL
)
```

## Arguments

- x:

  a character, the name of a numerical vector with discrete values -
  usually a factor.

- y:

  a character, the name of a numerical vector with discrete values -
  usually a factor.

- type:

  a character which represent the type of table to output: pooled table
  or one table for each data source or both. If `type` is set to
  'combine', a pooled 2-dimensional table is returned; If `type` is set
  to 'split' a 2-dimensional table is returned for each data source. If
  `type` is set to 'both' (default) a pooled 2-dimensional table plus a
  2-dimensional table for each data source are returned.

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

- colPercent:

  table(s) that hold column percentages for each level/category. Inner
  cells are reported as missing if one or more cells are 'invalid'.

- rowPercent:

  table(s) that hold row percentages for each level/category. Inner
  cells are reported as missing if one or more cells are 'invalid'.

- chi2Test:

  Chi-squared test for homogeneity.

- counts:

  table(s) that hold counts for each level/category. If some cell counts
  are invalid (see 'Details' section) only the total (outer) cell counts
  are displayed in the returned individual study tables or in the pooled
  table.

- validity:

  a text that informs the analyst about the validity of the output
  tables. If any tables are invalid the studies they are originated from
  are also mentioned in the text message.

## Details

The table returned by the server side function might be valid (non
disclosive - no table cell have counts between 1 and the minimal number
agreed by the data owner and set in the data repository as the
"nfilter.tab") or invalid (potentially disclosive - one or more table
cells have a count between 1 and the minimal number agreed by the data
owner). If a 2-dimensional table is invalid all the cells are set to NA
except the total counts. In this way, it is possible to combine total
counts across all the data sources but it is not possible to identify
the cell(s) that had the small counts which render the table invalid.

## See also

[ds.table1D](ds.table1D.md) for the tabulating one vector.

## Author

Amadou Gaye, Paul Burton, Demetris Avraam, for DataSHIELD Development
Team

## Examples
