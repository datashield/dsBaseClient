# Computes the absolute values of a variable

Computes the absolute values for a specified numeric or integer vector.
This function is similar to R function `abs`.

## Usage

``` r
ds.abs(x = NULL, newobj = NULL, datasources = NULL)
```

## Arguments

- x:

  a character string providing the name of a numeric or an integer
  vector.

- newobj:

  a character string that provides the name for the output variable that
  is stored on the data servers. Default name is set to `abs.newobj`.

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the `datasources` argument is not
  specified the default set of connections will be used: see
  [`datashield.connections_default`](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

## Value

`ds.abs` assigns a vector for each study that includes the absolute
values of the input numeric or integer vector specified in the argument
`x`. The created vectors are stored in the servers.

## Details

The function calls the server-side function `absDS` that computes the
absolute values of the elements of a numeric or integer vector and
assigns a new vector with those absolute values on the server-side. The
name of the new generated vector is specified by the user through the
argument `newobj`, otherwise is named by default to `abs.newobj`.

## Author

Demetris Avraam for DataSHIELD Development Team

Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands

## Examples

``` r
if (FALSE) { # \dontrun{

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
  
  # Example 1: Generate a normally distributed variable with zero mean and variance equal
  #  to one and then get their absolute values
  ds.rNorm(samp.size=100, mean=0, sd=1, newobj='var.norm', datasources=connections)
  # check the quantiles
  ds.summary(x='var.norm', datasources=connections)
  ds.abs(x='var.norm', newobj='var.norm.abs', datasources=connections)
  # check now the changes in the quantiles
  ds.summary(x='var.norm.abs', datasources=connections)  

  # Example 2: Generate a sequence of negative integer numbers from -200 to -100
  # and then get their absolute values
  ds.seq(FROM.value.char = '-200', TO.value.char = '-100', BY.value.char = '1', 
         newobj='negative.integers', datasources=connections)
  # check the quantiles
  ds.summary(x='negative.integers', datasources=connections)
  ds.abs(x='negative.integers', newobj='positive.integers', datasources=connections)
  # check now the changes in the quantiles
  ds.summary(x='positive.integers', datasources=connections)

  # clear the Datashield R sessions and logout
  datashield.logout(connections) 

} # }
```
