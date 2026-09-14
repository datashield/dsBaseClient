# Compute Weighted Mean by Group

This function is originally from the panelaggregation package. It has
been ported here in order to bypass the package being kicked off CRAN.

## Usage

``` r
computeWeightedMeans(data_table, variables, weight, by)
```

## Arguments

- data_table:

  a data.table

- variables:

  character name of the variable(s) to focus on. The variables must be
  in the data.table

- weight:

  character name of the data.table column that contains a weight.

- by:

  character vector of the columns to group by

## Value

Returns a data table object with computed weighted means.

## Author

Matthias Bannert, Gabriel Bucur
