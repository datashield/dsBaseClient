# Creates a repetitive sequence in the server-side

Creates a repetitive sequence by repeating the specified scalar number,
vector or list in each data source.

## Usage

``` r
ds.rep(
  x1 = NULL,
  times = NA,
  length.out = NA,
  each = 1,
  source.x1 = "clientside",
  source.times = NULL,
  source.length.out = NULL,
  source.each = NULL,
  x1.includes.characters = FALSE,
  newobj = NULL,
  datasources = NULL
)
```

## Arguments

- x1:

  an scalar number, vector or list.

- times:

  an integer from clientside or a serverside integer or vector.

- length.out:

  a clientside integer or a serverside integer or vector.

- each:

  a clientside or serverside integer.

- source.x1:

  the source `x1` argument. It can be "clientside" or "c" and serverside
  or "s".

- source.times:

  see `source.x1`

- source.length.out:

  see `source.x1`

- source.each:

  see `source.x1`

- x1.includes.characters:

  Boolean parameter which specifies if the `x1` is a character.

- newobj:

  a character string that provides the name for the output object that
  is stored on the data servers. Default `seq.vect`.

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the `datasources` argument is not
  specified the default set of connections will be used: see
  [`datashield.connections_default`](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

## Value

`ds.rep` returns in the server-side a vector with the specified
repetitive sequence.

## Details

All arguments that can denote in a clientside or a serverside (i.e.
`x1`, `times`, `length.out` or `each`).

Server function called: `repDS`.

## Author

DataSHIELD Development Team

Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands

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
    connections <- DSI::datashield.login(logins = logindata, 
                                         assign = TRUE, 
                                         symbol = "D") 

  # Creating a repetitive sequence  
              
     ds.rep(x1 = 4,
            times = 6,
            length.out = NA,
            each = 1,
            source.x1 = "clientside",
            source.times = "c",
            source.length.out = NULL,
            source.each = "c",
            x1.includes.characters = FALSE,
            newobj = "rep.seq",
            datasources = connections)
       
     ds.rep(x1 = "lung",
            times = 6,
            length.out = 7,
            each = 1,
            source.x1 = "clientside",
            source.times = "c",
            source.length.out = "c",
            source.each = "c",
            x1.includes.characters = TRUE,
            newobj = "rep.seq",
            datasources = connections)

  # Clear the Datashield R sessions and logout  
  datashield.logout(connections) 
} # } 
```
