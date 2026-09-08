# Lists client-side functions

Lists all current client-side functions

## Usage

``` r
ds.listClientsideFunctions()
```

## Value

`ds.listClientsideFunctions` returns a list containing all server-side
functions.

## Details

This function operates by directly interrogating the R objects stored in
the input client packages and objects of name starting with `ds.`
character in `.GlobalEnv`.

This function does not call any server-side function.

## Author

DataSHIELD Development Team

## Examples

``` r
if (FALSE) { # \dontrun{
  ## Version 6, for version 5 see the Wiki
  
  #Library with all DataSHIELD functions
  require('dsBaseClient')
  
  #Visualise all functions
  ds.listClientsideFunctions()
  
} # }   
```
