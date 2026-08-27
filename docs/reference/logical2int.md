# Turns a logical operator into an integer

This is an internal function.

## Usage

``` r
logical2int(obj = NULL)
```

## Arguments

- obj:

  a character, the logical parameter to turn into an integer

## Value

an integer

## Details

This function is called to turn a logical operator given as a character
into an integer: '\>' is turned into 1, '\>=' into 2, '\<' into 3, '\<='
into 4, '==' into 5 and '!=' into 6.
