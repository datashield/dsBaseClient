# Lists disclosure settings

Lists current values for disclosure control filters in all data
repository servers.

## Usage

``` r
ds.listDisclosureSettings(datasources = NULL)
```

## Arguments

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the `datasources` argument is not
  specified the default set of connections will be used: see
  [`datashield.connections_default`](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

## Value

`ds.listDisclosureSettings` returns a list containing the current
settings of the `nfilters` in each study specified.

## Details

This function lists out the current values of the eight disclosure
filters in each of the data repository servers specified by
`datasources` argument.  

The eight filters are explained below:

\(1\) `nfilter.tab`, the minimum non-zero cell count allowed in any cell
if a contingency table is to be returned. This applies to one
dimensional and two dimensional tables of counts tabulated across one or
two factors and to tables of a mean of a quantitative variable tabulated
across a factor. Default usually set to 3 but a value of 1 (no limit)
may be necessary, particularly if low cell counts are highly probable
such as when working with rare diseases. Five is also a justifiable
choice to replicate the most common threshold rule imposed by data
releasers worldwide, but it should be recognised that many census
providers are moving to ten - but the formal justification of this is
little more than 'it is safer' and everybody is scared of something
going wrong - in practice it is very easy to get around any block and so
it is debatable whether the scientific cost outweighs the imposition of
any threshold.

\(2\) `nfilter.subset`, the minimum non-zero count of observational
units (typically individuals) in a subset. Typically defaulted to 3.

\(3\) `nfilter.glm`, the maximum number of parameters in a regression
model as a proportion of the sample size in a study. If a study has 1000
observational units (typically individuals) being used in a particular
analysis then if `nfilter.glm` is set to 0.33 (its default value) the
maximum allowable number of parameters in a model fitted to those data
will be 330. This disclosure filter protects against fitting overly
saturated models that can be disclosive. The choice of 0.33 is entirely
arbitrary.

\(4\) `nfilter.string`, the maximum length of a string argument if that
argument is to be subject to testing of its length. Default value 80.
The aim of this `nfilter` is to make it difficult for hackers to find a
way to embed malicious code in a valid string argument that is actively
interpreted.

\(5\) `nfilter.string`, Short to be used when a string must be specified
but that when valid that string should be short.

\(6\) `nfilter.kNN` applies to graphical plots based on working with the
k nearest neighbours of each point. `nfilter.kNN` specifies the minimum
allowable value for the number of nearest neighbours used, typically
defaulted to 3.

\(7\) `nfilter.levels` specifies the maximum number of unique levels of
a factor variable that can be disclosed to the client. In the absence of
this filter a user can convert a numeric variable to a factor and see
its unique levels which are all the distinct values of the numeric
vector. To prevent such disclosure we set this threshold to 0.33 which
ensures that if a factor has unique levels more than the 33

\(8\) `nfilter.noise` specifies the minimum level of noise added in some
variables mainly used for data visualizations. The default value is 0.25
which means that the noise added to a given variable, follows a normal
distribution with zero mean and variance equal to 25 variance of the
given variable. Any value greater than this threshold can reduce the
risk of disclosure.

Server function called: `listDisclosureSettingsDS`

## Author

DataSHIELD Development Team

## Examples

``` r
if (FALSE) { # \dontrun{
 
  ## Version 6, for version 5 see Wiki
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
  
  # Call to list current disclosure settings in all data repository servers 
  
  ds.listDisclosureSettings(datasources = connections)
  
  # Restrict call to list disclosure settings only to the first, or second DS connection (study)
  
  ds.listDisclosureSettings(datasources = connections[1]) 
  ds.listDisclosureSettings(datasources = connections[2])
            
   # Clear the Datashield R sessions and logout  
   datashield.logout(connections) 
} # }
```
