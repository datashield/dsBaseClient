# Computes the WHO Growth Reference z-scores of anthropometric data

Calculate WHO Growth Reference z-score for a given anthropometric
measurement This function is similar to R function `getWGSR` from the
`zscorer` package.

## Usage

``` r
ds.getWGSR(
  sex = NULL,
  firstPart = NULL,
  secondPart = NULL,
  index = NULL,
  standing = NA,
  thirdPart = NA,
  newobj = NULL,
  datasources = NULL
)
```

## Arguments

- sex:

  the name of the binary variable that indicates the sex of the subject.
  This must be coded as 1 = male and 2 = female. If in your project the
  variable sex has different levels, you should recode the levels to 1
  for males and 2 for females using the `ds.recodeValues` DataSHIELD
  function before the use of the `ds.getWGSR`.

- firstPart:

  Name of variable specifying:  
  Weight (kg) for BMI/A, W/A, W/H, or W/L  
  Head circumference (cm) for HC/A  
  Height (cm) for H/A  
  Length (cm) for L/A  
  MUAC (cm) for MUAC/A  
  Sub-scapular skinfold (mm) for SSF/A  
  Triceps skinfold (mm) for TSF/A  
  Give a quoted variable name as in (e.g.) "weight". Be careful with
  units (weight in kg; height, length, head circumference, and MUAC in
  cm; skinfolds in mm).

- secondPart:

  Name of variable specifying:  
  Age (days) for H/A, HC/A, L/A, MUAC/A, SSF/A, or TSF/A  
  Height (cm) for BMI/A, or W/H  
  Length (cm) for W/L  
  Give a quoted variable name as in (e.g.) "age". Be careful with units
  (age in days; height and length in cm).

- index:

  The index to be calculated and added to data. One of:  
  bfa BMI for age  
  hca Head circumference for age  
  hfa Height for age  
  lfa Length for age  
  mfa MUAC for age  
  ssa Sub-scapular skinfold for age  
  tsa Triceps skinfold for age  
  wfa Weight for age  
  wfh Weight for height  
  wfl Weight for length  
  Give a quoted index name as in (e.g.) "wfh".

- standing:

  Variable specifying how stature was measured. If NA (default) then age
  (for "hfa" or "lfa") or height rules (for "wfh" or "wfl") will be
  applied. This must be coded as 1 = Standing; 2 = Supine; 3 = Unknown.
  Missing values will be recoded to 3 = Unknown. Give a single value
  (e.g."1"). If no value is specified then height and age rules will be
  applied.

- thirdPart:

  Name of variable specifying age (in days) for BMI/A. Give a quoted
  variable name as in (e.g.) "age". Be careful with units (age in days).
  If age is given in different units you should convert it in age in
  days using the `ds.make` DataSHIELD function before the use of the
  `ds.getWGSR`. For example if age is given in months then the
  transformation is given by the formula
  \$age_days=age_months\*(365.25/12)\$.

- newobj:

  a character string that provides the name for the output variable that
  is stored on the data servers. Defaults `getWGSR.newobj`.

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the `datasources` argument is not
  specified the default set of connections will be used: see
  [`datashield.connections_default`](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

## Value

`ds.getWGSR` assigns a vector for each study that includes the z-scores
for the specified index. The created vectors are stored in the servers.

## Details

The function calls the server-side function `getWGSRDS` that computes
the WHO Growth Reference z-scores of anthropometric data for weight,
height or length, MUAC (middle upper arm circumference), head
circumference, sub-scapular skinfold and triceps skinfold. Note that the
function might fail or return NAs when the variables are outside the
ranges given in the WGS (WHO Child Growth Standards) reference (i.e. 45
to 120 cm for height and 0 to 60 months for age). It is up to the user
to check the ranges and the units of their data.

## Author

Demetris Avraam for DataSHIELD Development Team

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
                 table = "ANTHRO.anthro1", driver = "OpalDriver")
  builder$append(server = "study2", 
                 url = "http://192.168.56.100:8080/", 
                 user = "administrator", password = "datashield_test&", 
                 table = "ANTHRO.anthro2", driver = "OpalDriver")
  builder$append(server = "study3",
                 url = "http://192.168.56.100:8080/", 
                 user = "administrator", password = "datashield_test&", 
                 table = "ANTHRO.anthro3", driver = "OpalDriver")
                 
  logindata <- builder$build()
  
  # Log onto the remote Opal training servers
  connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D") 
  
  # Example 1: Generate the weight-for-height (wfh) index
  ds.getWGSR(sex = "D$sex", firstPart = "D$weight", secondPart = "D$height",
           index = "wfh", newobj = "wfh_index", datasources = connections)

  # Example 2: Generate the BMI for age (bfa) index
  ds.getWGSR(sex = "D$sex", firstPart = "D$weight", secondPart = "D$height",
           index = "bfa", thirdPart = "D$age", newobj = "bfa_index", datasources = connections)

  # clear the Datashield R sessions and logout
  datashield.logout(connections) 

} # }
```
