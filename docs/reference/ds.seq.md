# Generates a sequence in the server-side

This function generates a sequence for given parameters on the
server-side.

## Usage

``` r
ds.seq(
  FROM.value.char = "1",
  BY.value.char = "1",
  TO.value.char = NULL,
  LENGTH.OUT.value.char = NULL,
  ALONG.WITH.name = NULL,
  newobj = "newObj",
  datasources = NULL
)
```

## Arguments

- FROM.value.char:

  an integer or a number in character from specifying the starting value
  for the sequence. Default `"1"`.

- BY.value.char:

  an integer or a number in character from specifying the value to
  increment each step in the sequence. Default `"1"`.

- TO.value.char:

  an integer or a number in character from specifying the terminal value
  for the sequence. Default NULL. For more information see **Details**.

- LENGTH.OUT.value.char:

  an integer or a number in character from specifying the length of the
  sequence at which point its extension should be stopped. Default NULL.
  For more information see **Details**.

- ALONG.WITH.name:

  a character string specifying the name of a standard vector to
  generate a vector of the same length. For more information see
  **Details**.

- newobj:

  a character string that provides the name for the output variable that
  is stored on the data servers. Default `seq.newobj`.

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the `datasources` argument is not
  specified the default set of connections will be used: see
  [`datashield.connections_default`](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

## Value

`ds.seq` returns to the server-side the generated sequence. Also, two
validity messages are returned to the client-side indicating whether the
new object has been created in each data source and if so whether it is
in a valid form.

## Details

This function is similar to a native R function
[`seq()`](https://rdrr.io/r/base/seq.html). It creates a flexible range
of sequence vectors that can then be used to help manage and analyse
data.

**Note**: the combinations of arguments that are not allowed for the
function `seq` in native R are also prohibited in `ds.seq`.

To be specific, `FROM.value.char` argument defines the start of the
sequence and `BY.value.char` defines how the sequence is incremented (or
decremented) at each step. But where the sequence stops can be defined
in three different ways:  
(1) `TO.value.char` indicates the terminal value of the sequence. For
example,
`ds.seq(FROM.value.char = "3", BY.value.char = "2", TO.value.char = "7")`
creates the sequence `3,5,7` on the server-side.  
(2) `LENGTH.OUT.value.char` indicates the length of the sequence. For
example,
`ds.seq(FROM.value.char = "3", BY.value.char = "2", LENGTH.OUT.value.char = "7")`
creates the sequence `3,5,7,9,11,13,15` on the server-side.  
(3) `ALONG.WITH.name` specifies the name of a variable on the
server-side, such that the sequence in each study will be equal in
length to that variable. For example,
`ds.seq(FROM.value.char = "3", BY.value.char = "2", ALONG.WITH.name = "var.x")`
creates a sequence such that if `var.x` is of length 100 in study 1 the
sequence written to study 1 will be `3,5,7,...,197,199,201` and if
`var.x` is of length 4 in study 2, the sequence written to study 2 will
be `3,5,7,9`.  
Only one of the three arguments: `TO.value.char`,
`LENGTH.OUT.value.char` and `ALONG.WITH.name` can be non-null in any one
call.

In `LENGTH.OUT.value.char` argument if you specify a number with a
decimal point but in character form this result in a sequence
`length(integer) + 1`. For example,
`LENGTH.OUT.value.char = "1000.0001"` generates a sequence of length
1001.

Server function called: `seqDS`

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

  #Create 3 different sequences
  
  ds.seq(FROM.value.char = "1",
         BY.value.char = "2",
         TO.value.char = "7",
         newobj = "new.seq1",
         datasources = connections)
         
         
  ds.seq(FROM.value.char = "4",
         BY.value.char = "3",
         LENGTH.OUT.value.char = "10",
         newobj = "new.seq2",
         datasources = connections)  
         
  ds.seq(FROM.value.char = "2",
         BY.value.char = "5",
         ALONG.WITH.name = "D$GENDER",
         newobj = "new.seq3",
         datasources = connections)                            
         
  # Clear the Datashield R sessions and logout
  datashield.logout(connections) 
} # }
```
