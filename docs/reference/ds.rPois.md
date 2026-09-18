# Generates Poisson distribution in the server-side

Generates random (pseudorandom) non-negative integers with a Poisson
distribution. Besides, `ds.rPois` allows creating different vector
lengths in each server.

## Usage

``` r
ds.rPois(
  samp.size = 1,
  lambda = 1,
  newobj = "newObject",
  seed.as.integer = NULL,
  return.full.seed.as.set = FALSE,
  datasources = NULL
)
```

## Arguments

- samp.size:

  an integer value or an integer vector that defines the length of the
  random numeric vector to be created in each source.

- lambda:

  the number of events mean per interval.

- newobj:

  a character string that provides the name for the output variable that
  is stored on the data servers. Default `newObject`.

- seed.as.integer:

  an integer or a NULL value which provides the random seed in each data
  source.

- return.full.seed.as.set:

  logical, if TRUE will return the full random number seed in each data
  source (a numeric vector of length 626). If FALSE it will only return
  the trigger seed value you have provided. Default is FALSE.

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the `datasources` argument is not
  specified the default set of connections will be used: see
  [`datashield.connections_default`](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

## Value

`ds.rPois` returns random number vectors with a Poisson distribution for
each study, taking into account the values specified in each parameter
of the function. The created vectors are stored in the server-side. If
requested, it also returned to the client-side the full 626 lengths
random seed vector generated in each source (see info for the argument
`return.full.seed.as.set`).

## Details

Creates a vector of random or pseudorandom non-negative integer values
distributed with a Poisson distribution in each data source. The
`ds.rPois` function's arguments specify lambda, the length and the seed
of the output vector in each source.

To specify different `lambda` value in each source, you can use a
character vector `(..., lambda = "vector.of.lambdas"...)` or the
`datasources` parameter to create the random vector for one source at a
time, changing `lambda` as required. Default value for `lambda> = 1`.

If `seed.as.integer` is an integer e.g. 5 and there is more than one
source (N) the seed is set as 5\*N. For example, in the first study the
seed is set as 938\*1, in the second as 938\*2 up to 938\*N in the Nth
study.

If `seed.as.integer` is set as 0 all sources will start with the seed
value 0 and all the random number generators will, therefore, start from
the same position. Also, to use the same starting seed in all studies
but do not wish it to be 0, you can use `datasources` argument to
generate the random number vectors one source at a time.

Server functions called: `rPoisDS` and `setSeedDS`.

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

  # Generating the vectors in the Opal servers
  ds.rPois(samp.size=c(13,20,25), #the length of the vector created in each source is different
          lambda=as.character(c(2,3,4)), #different mean per interval (2,3,4) in each source
          newobj="Pois.dist",                   
          seed.as.integer=1234,         
          return.full.seed.as.set=FALSE, 
          datasources=connections)  #all the Opal servers are used, in this case 3 
                                    #(see above the connection to the servers) 
  ds.rPois(samp.size=13,                
          lambda=5,
          newobj="Pois.dist", 
          seed.as.integer=1234, 
          return.full.seed.as.set=FALSE, 
          datasources=connections[1])  #only the first Opal server is used ("study1")
        
  # Clear the Datashield R sessions and logout
  datashield.logout(connections) 
} # }
```
