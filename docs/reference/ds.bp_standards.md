# Calculates Blood pressure z-scores

The function calculates blood pressure z-scores in two steps: Step 1.
Calculates z-score of height according to CDC growth chart (Not the WHO
growth chart!). Step 2. Calculates z-score of BP according to the fourth
report on BP management, USA

## Usage

``` r
ds.bp_standards(
  sex = NULL,
  age = NULL,
  height = NULL,
  bp = NULL,
  systolic = TRUE,
  newobj = NULL,
  datasources = NULL
)
```

## Arguments

- sex:

  the name of the sex variable. The variable should be coded as 1 for
  males and 2 for females. If it is coded differently (e.g. 0/1), then
  you can use the ds.recodeValues function to recode the categories to
  1/2 before the use of ds.bp_standards

- age:

  the name of the age variable in years.

- height:

  the name of the height variable in cm.

- bp:

  the name of the blood pressure variable.

- systolic:

  logical. If TRUE (default) the function assumes conversion of systolic
  blood pressure. If FALSE the function assumes conversion of diastolic
  blood pressure.

- newobj:

  a character string that provides the name for the output object that
  is stored on the data servers. Default name is set to `bp.newobj`.

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the `datasources` argument is not
  specified the default set of connections will be used: see
  [`datashield.connections_default`](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

## Value

assigns a new object on the server-side. The assigned object is a list
with two elements: the 'Zbp' which is the zscores of the blood pressure
and 'perc' which is the percentiles of the BP zscores.

## References

The fourth report on the diagnosis, evaluation, and treatment of high
blood pressure in children and adolescents:
https://www.nhlbi.nih.gov/sites/default/files/media/docs/hbp_ped.pdf

## Author

Demetris Avraam for DataSHIELD Development Team
