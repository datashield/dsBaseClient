# Converts birth measurements to intergrowth z-scores/centiles

Converts birth measurements to INTERGROWTH z-scores/centiles (generic)

## Usage

``` r
ds.igb_standards(
  gagebrth = NULL,
  z = 0,
  p = 50,
  val = NULL,
  var = NULL,
  sex = NULL,
  fun = "igb_value2zscore",
  newobj = NULL,
  datasources = NULL
)
```

## Arguments

- gagebrth:

  the name of the "gestational age at birth in days" variable.

- z:

  z-score(s) to convert (must be between 0 and 1). Default value is 0.
  This value is used only if `fun` is set to "igb_zscore2value".

- p:

  centile(s) to convert (must be between 0 and 100). Default value is
  p=50. This value is used only if `fun` is set to "igb_centile2value".

- val:

  the name of the anthropometric variable to convert.

- var:

  the name of the measurement to convert ("lencm", "wtkg", "hcircm",
  "wlr").

- sex:

  the name of the sex factor variable. The variable should be coded as
  Male/Female. If it is coded differently (e.g. 0/1), then you can use
  the ds.recodeValues function to recode the categories to Male/Female
  before the use of ds.igb_standards.

- fun:

  the name of the function to be used. This can be one of:
  "igb_centile2value", "igb_zscore2value", "igb_value2zscore" (default),
  "igb_value2centile".

- newobj:

  a character string that provides the name for the output variable that
  is stored on the data servers. Default name is set to `igb.newobj`.

- datasources:

  a list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the `datasources` argument is not
  specified the default set of connections will be used: see
  [`datashield.connections_default`](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

## Value

assigns the converted measurement as a new object on the server-side

## Note

For gestational ages between 24 and 33 weeks, the INTERGROWTH very early
preterm standard is used.

## References

- Villar, J., Ismail, L.C., Victora, C.G., Ohuma, E.O., Bertino, E.,
  Altman, D.G., Lambert, A., Papageorghiou, A.T., Carvalho, M., Jaffer,
  Y.A., Gravett, M.G., Purwar, M., Frederick, I.O., Noble, A.J., Pang,
  R., Barros, F.C., Chumlea, C., Bhutta, Z.A., Kennedy, S.H., 2014.
  International standards for newborn weight, length, and head
  circumference by gestational age and sex: the Newborn Cross-Sectional
  Study of the INTERGROWTH-21st Project. The Lancet 384, 857–868.
  https://doi.org/10.1016/S0140-6736(14)60932-6

- Villar, J., Giuliani, F., Fenton, T.R., Ohuma, E.O., Ismail, L.C.,
  Kennedy, S.H., 2016. INTERGROWTH-21st very preterm size at birth
  reference charts. The Lancet 387, 844–845.
  https://doi.org/10.1016/S0140-6736(16)00384-6

## Author

Demetris Avraam for DataSHIELD Development Team
