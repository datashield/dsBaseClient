# Generates Uniform distribution in the server-side

Generates uniformly distributed random (pseudorandom) scalar numbers.
Besides, `ds.rUnif` allows creating different vector lengths in each
server.

## Usage

``` r
ds.rUnif(
  samp.size = 1,
  min = 0,
  max = 1,
  newobj = "newObject",
  seed.as.integer = NULL,
  return.full.seed.as.set = FALSE,
  force.output.to.k.decimal.places = 9,
  datasources = NULL
)
```

## Arguments

- samp.size:

  an integer value or an integer vector that defines the length of the
  random numeric vector to be created in each source.

- min:

  a numeric scalar that specifies the minimum value of the random
  numbers in the distribution.

- max:

  a numeric scalar that specifies the maximum value of the random
  numbers in the distribution.

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

- force.output.to.k.decimal.places:

  an integer or an integer vector that forces the output random numbers
  vector to have k decimals.

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the `datasources` argument is not
  specified the default set of connections will be used: see
  [`datashield.connections_default`](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

## Value

`ds.Unif` returns random number vectors with a uniform distribution for
each study, taking into account the values specified in each parameter
of the function. The created vectors are stored in the server-side. If
requested, it also returned to the client-side the full 626 lengths
random seed vector generated in each source (see info for the argument
`return.full.seed.as.set`).

## Details

It creates a vector of pseudorandom numbers distributed with a uniform
probability in each data source. The `ds.Unif` function's arguments
specify the minimum and maximum of the uniform distribution and the
length and the seed of the output vector in each source.

To specify different `min` values in each source, you can use a
character vector `(..., min="vector.of.mins"...)` or the `datasources`
parameter to create the random vector for one source at a time, changing
the `min` value as required. Default value for `min = 0`.

To specify different `max` values in each source, you can use a
character vector `(..., max="vector.of.maxs"...)` or the `datasources`
parameter to create the random vector for one source at a time, changing
the `max` value as required. Default value for `max = 1`.

If `seed.as.integer` is an integer e.g. 5 and there is more than one
source (N) the seed is set as 5\*N. For example, in the first study the
seed is set as 938\*1, in the second as 938\*2 up to 938\*N in the Nth
study.

If `seed.as.integer` is set as 0 all sources will start with the seed
value 0 and all the random number generators will, therefore, start from
the same position. Also, to use the same starting seed in all studies
but do not wish it to be 0, you can use `datasources` argument to
generate the random number vectors one source at a time.

In `force.output.to.k.decimal.places` the range of k is 1-8 decimals. If
`k = 0` the output random numbers are forced to an integer. If `k = 9`,
no rounding of output numbers occurs. The default value of
`force.output.to.k.decimal.places = 9`. If you wish to generate integers
with equal probabilities in the range 1-10 you should specify
`min = 0.5` and `max = 10.5`. Default value for `k = 9`.

Server functions called: `rUnifDS` and `setSeedDS`.

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

  ds.rUnif(samp.size = c(12,20,4), #the length of the vector created in each source is different 
           min = as.character(c(0,2,5)), #different minumum value of the function in each source
           max = as.character(c(2,5,9)), #different maximum value of the function in each source
           newobj = "Unif.dist",
           seed.as.integer = 234,
           return.full.seed.as.set = FALSE,
           force.output.to.k.decimal.places = c(1,2,3),
           datasources = connections)   #all the Opal servers are used, in this case 3 
                                        #(see above the connection to the servers) 

  ds.rUnif(samp.size = 12,
           min = 0,
           max = 2,
           newobj = "Unif.dist",
           seed.as.integer = 12345,
           return.full.seed.as.set = FALSE,
           force.output.to.k.decimal.places = 2,
           datasources = connections[2]) #only the second  Opal server is used ("study2")
           
  # Clear the Datashield R sessions and logout           
  datashield.logout(connections)
} # }
 
```
