# Display missing data patterns with disclosure control

This function is a client-side wrapper for the server-side mdPatternDS
function. It generates a missing data pattern matrix similar to
mice::md.pattern but with disclosure control applied to prevent
revealing small cell counts.

## Usage

``` r
ds.mdPattern(x = NULL, type = "split", datasources = NULL)
```

## Arguments

- x:

  a character string specifying the name of a data frame or matrix on
  the server-side containing the data to analyze.

- type:

  a character string specifying the output type. If 'split' (default),
  returns separate patterns for each study. If 'combine', attempts to
  pool patterns across studies.

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the `datasources` argument is not
  specified, the default set of connections will be used: see
  [`datashield.connections_default`](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

## Value

For type='split': A list with one element per study, each containing:

- pattern:

  The missing data pattern matrix for that study

- valid:

  Logical indicating if all patterns meet disclosure requirements

- message:

  A message describing the validity status

For type='combine': A list containing:

- pattern:

  The pooled missing data pattern matrix across all studies

- valid:

  Logical indicating if all pooled patterns meet disclosure requirements

- message:

  A message describing the validity status

## Details

The function calls the server-side mdPatternDS function which uses
mice::md.pattern to analyze missing data patterns. Patterns with counts
below the disclosure threshold (default: nfilter.tab = 3) are suppressed
to maintain privacy.

**Output Format:** - Each row represents a missing data pattern -
Pattern counts are shown in row names (e.g., "150", "25") - Columns show
1 if the variable is observed, 0 if missing - Last column shows the
total number of missing values per pattern - Last row shows the total
number of missing values per variable

**Disclosure Control:**

Suppressed patterns (count below threshold) are indicated by: - Row
name: "suppressed(\<N\>)" where N is the threshold - All pattern values
set to NA - Summary row also suppressed to prevent back-calculation

**Pooling Behavior (type='combine'):**

When pooling across studies, the function uses a *conservative approach*
for disclosure control:

1\. Identifies identical missing patterns across studies 2. **EXCLUDES
suppressed patterns from pooling** - patterns suppressed in ANY study
are not included in the pooled count 3. Sums counts only for
non-suppressed identical patterns 4. Re-validates pooled counts against
disclosure threshold

**Important:** This conservative approach means: - Pooled counts may be
*underestimates* if some studies had suppressed patterns - This prevents
disclosure through subtraction (e.g., if study A shows count=5 and pool
shows count=7, one could deduce study B has count=2, violating
disclosure) - Different patterns across studies are preserved separately
in the pooled result

## Author

Xavier Escribà montagut for DataSHIELD Development Team

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
  logindata <- builder$build()

  connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D")

  # Get missing data patterns for each study separately
  patterns_split <- ds.mdPattern(x = "D", type = "split", datasources = connections)

  # View results for study1
  print(patterns_split$study1$pattern)
  #      var1 var2 var3
  # 150    1    1    1  0    <- 150 obs complete
  #  25    0    1    1  1    <- 25 obs missing var1
  #       25    0    0 25    <- Summary: 25 missing per variable

  # Get pooled missing data patterns across studies
  patterns_pooled <- ds.mdPattern(x = "D", type = "combine", datasources = connections)
  print(patterns_pooled$pattern)

  # Example with suppressed patterns:
  # If study1 has a pattern with count=2 (suppressed) and study2 has same pattern 
  # with count=5 (valid), the pooled result will show count=5 (conservative approach)
  # A warning will indicate: "Pooled counts may underestimate the true total"

  # Clear the Datashield R sessions and logout
  datashield.logout(connections)
} # }
```
