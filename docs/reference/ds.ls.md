# lists all objects on a server-side environment

creates a list of the names of all of the objects in a specified
serverside environment.

## Usage

``` r
ds.ls(
  search.filter = NULL,
  env.to.search = 1L,
  search.GlobalEnv = TRUE,
  datasources = NULL
)
```

## Arguments

- search.filter:

  character string (potentially including `*` symbol) specifying the
  filter for the object name that you want to find in the environment.
  For more information see **Details**.

- env.to.search:

  an integer (e.g. in `2` or `2L` format) specifying the position in the
  search path of the environment to be explored. `1L` is the current
  active analytic environment on the server-side and is the default
  value of `env.to.search`. For more information see **Details**.

- search.GlobalEnv:

  Logical. If TRUE, `ds.ls` will list all objects in the `.GlobalEnv` R
  environment on the server-side. If FALSE and if `env.to.search` is
  also set as a valid integer, `ds.ls` will list all objects in the
  server-side R environment identified by `env.to.search` in the search
  path. For more information see **Details**.

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the `datasources` argument is not
  specified the default set of connections will be used: see
  [`datashield.connections_default`](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

## Value

`ds.ls` returns to the client-side a list containing:  
(1) the name/details of the server-side R environment which `ds.ls` has
searched;  
(2) a vector of character strings giving the names of all objects
meeting the naming criteria specified by the argument `search.filter` in
this specified R server-side environment;  
(3) the nature of the search filter string as it was applied.

## Details

When running analyses one may want to know the objects already
generated. This request is not disclosive as it only returns the names
of the objects and not their contents.

By default, objects in DataSHIELD's Active Serverside Analytic
Environment (`.GlobalEnv`) will be listed. This is the environment that
contains all of the objects that server-side DataSHIELD is using for the
main analysis or has written out to the server-side during the process
of managing or undertaking the analysis (variables, scalars, matrices,
data frames, etc).

The environment to explore is specified by the argument `env.to.search`
(i.e. environment to search) to an integer value. The default
environment which R names as `.GlobalEnv` is set by specifying
`env.to.search = 1` or `1L` (`1L` is just an explicit way of writing the
integer `1`).

If the `search.GlobalEnv` argument is set to TRUE the `env.to.search`
parameter is set to `1L` regardless of what value it is set in the call
or if it is set to NULL. So, if `search.GlobalEnv` is set to TRUE,
`ds.ls` will automatically search the `.GlobalEnv` R environment on the
server-side which contains all of the variables, data frames and other
objects read in at the start of the analysis, as well as any new objects
of any sort created using DataSHIELD assign functions.

Other server-side environments contain other objects. For example,
environment `2L` contains the functions loaded via the native R stats
package and `6L` contains the standard list of datasets built into R. By
default `ds.ls` will return a list of ALL of the objects in the
environment specified by the `env.to.search` argument but you can
specify search filters including `*` wildcards using the `search.filter`
argument.

In `search.filter` you can use the symbol `*` to find all the object
that contains the specified characters. For example,
`search.filter = "Sd2*"` will list the names of all objects in the
specified environment with names beginning capital S, lower case d and
number 2. Similarly, `search.filter="*.ID"` will return all objects with
names ending with `.ID`, for example `Study.ID`. If a value is not
specified for the `search.filter` argument or it is set as NULL, the
names of all objects in the specified environment will be returned.

Server function called: `lsDS`.

## Author

DataSHIELD Development Team

Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands

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
  
  connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D") 
  
  #Example 1: Obtain the list of  all objects on a server-side environment
  
  ds.ls(datasources = connections)
  
  #Example 2: Obtain the list of all objects that contain "var" character in the name
  #Create in the server-side variables with "var" character in the name
  
  ds.assign(toAssign = "D$LAB_TSC",
            newobj = "var.LAB_TSC",
            datasources = connections)
  ds.assign(toAssign = "D$LAB_TRIG",
            newobj = "var.LAB_TRIG",
            datasources = connections)
  ds.assign(toAssign = "D$LAB_HDL",
            newobj = "var.LAB_HDL",
            datasources = connections)
  
  ds.ls(search.filter = "var*",
        env.to.search = 1L,
        search.GlobalEnv = TRUE,
        datasources = connections)
  
  # clear the Datashield R sessions and logout
  datashield.logout(connections)
} # }
```
