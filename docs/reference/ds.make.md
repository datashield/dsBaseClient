# Calculates a new object in the server-side

This function defines a new object in the server-side via an allowed
function or an arithmetic expression.

`ds.make` function is equivalent to `ds.assign`, but runs slightly
faster.

## Usage

``` r
ds.make(toAssign = NULL, newobj = NULL, datasources = NULL)
```

## Arguments

- toAssign:

  a character string specifying the function or the arithmetic
  expression.

- newobj:

  a character string that provides the name for the output variable that
  is stored on the data servers. Default `make.newobj`.

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the `datasources` argument is not
  specified the default set of connections will be used: see
  [`datashield.connections_default`](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

## Value

`ds.make` returns the new object which is written to the server-side.
Also a validity message is returned to the client-side indicating
whether the new object has been correctly created at each source.

## Details

If the new object is created successfully, the function will verify its
existence on the required servers. Please note there are certain modes
of failure where it is reported that the object has been created but it
is not there. This reflects a failure in the processing of some sort and
warrants further exploration of the details of the call to `ds.make` and
the variables/objects which it invokes.

TROUBLESHOOTING: please note we have recently identified an error that
makes `ds.make` fail and DataSHIELD crash.

The error arises from a call such as
`ds.make(toAssign = '5.3 + beta*xvar', newobj = 'predvals')`. This is a
typical call you may make to get the predicted values from a simple
linear regression model where a `y` variable is regressed against an `x`
variable (`xvar`) where the estimated regression intercept is `5.3` and
`beta` is the estimated regression slope.

This call appears to fail because in interpreting the arithmetic
function which is its first argument it first encounters the (length 1)
scalar `5.3` and when it then encounters the `xvar` vector which has
more than one element it fails - apparently because it does not
recognise that you need to replicate the `5.3` value the appropriate
number of times to create a vector of length equal to `xvar` with each
value equal to `5.3`.

There are two work-around solutions here:

\(1\) explicitly create a vector of appropriate length with each value
equal to `5.3`. To do this there is a useful trick. First identify a
convenient numeric variable with no missing values (typically a numeric
individual ID) let us call it `indID` equal in length to `xvar` (`xvar`
may include NAs but that doesn't matter provided `indID` is the same
total length). Then issue the call
`ds.make(toAssign = 'indID-indID+1',newobj = 'ONES')`. This creates a
vector of ones (called `ONES`) in each source equal in length to the
`indID` vector in that source. Then issue the second call
`ds.make(toAssign = 'ONES*5.3',newobj = 'vect5.3')` which creates the
required vector of length equal to `xvar` with all elements `5.3`.
Finally, you can now issue a modified call to reflect what was
originally needed:
`ds.make(toAssign = 'vect5.3+beta*xvar', 'predvals')`.

\(2\) Alternatively, if you simply swap the original call around:
`ds.make(toAssign = '(beta*xvar)+5.3', newobj = 'predvals')` the error
seems also to be circumvented. This is presumably because the first
element of the arithmetic function is of length equal to `xvar` and it
then knows to replicate the `5.3` that many times in the second part of
the expression.

The second work-around is easier, but it is worth knowing about the
first trick because creating a vector of ones of equal length to another
vector can be useful in other settings. Equally the call:
`ds.make(toAssign = 'indID-indID',newobj = 'ZEROS')` to create a vector
of zeros of that same length may also be useful.

Server function : `messageDS`

The `ds.make` function is a wrapper for the DSI package function
`datashield.assign`

## Author

DataSHIELD Development Team

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
                 table = "SURVIVAL.EXPAND_NO_MISSING1", driver = "OpalDriver")
  builder$append(server = "study2", 
                 url = "http://192.168.56.100:8080/", 
                 user = "administrator", password = "datashield_test&", 
                 table = "SURVIVAL.EXPAND_NO_MISSING2", driver = "OpalDriver")
  builder$append(server = "study3",
                 url = "http://192.168.56.100:8080/", 
                 user = "administrator", password = "datashield_test&", 
                 table = "SURVIVAL.EXPAND_NO_MISSING3", driver = "OpalDriver")
  logindata <- builder$build()
  
  connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D") 
  
##Example 1: arithmetic operators 

ds.make(toAssign = "D$age.60 + D$bmi.26", 
        newobj = "exprs1", 
        datasources = connections)
        
ds.make(toAssign = "D$noise.56 + D$pm10.16",
        newobj = "exprs2", 
        datasources = connections)
        
ds.make(toAssign = "(exprs1*exprs2)/3.2",
        newobj = "result.example1", 
        datasources = connections)

##Example 2: miscellaneous operators within functions

ds.make(toAssign = "(D$female)^2",
        newobj = "female2",
       datasources = connections)
       
ds.make(toAssign = "(2*D$female)+(D$log.surv)-(female2*2)",
        newobj = "output.test.1",
        datasources = connections)
        
ds.make(toAssign = "exp(output.test.1)",
        newobj = "output.test",
       datasources = connections)
       
  # clear the Datashield R sessions and logout
  datashield.logout(connections)
} # }
```
