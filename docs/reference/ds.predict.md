# ds.predict

Generates server-side predictions using the client-side output from
`ds.glm`.

## Usage

``` r
ds.predict(
  name = NULL,
  newdataname = NULL,
  type = c("response", "link"),
  newobj = "new.predictions",
  traindataname = NULL,
  na.action = "na.pass",
  datasources = NULL
)
```

## Arguments

- name:

  The client-side return object from `ds.glm`.

- newdataname:

  A character string specifying the name of the new dataset to be used
  for predictions.

- type:

  A character string specifying the type of prediction. Options are
  `"response"` or `"link"`.

- newobj:

  A character string specifying the name of the output object created on
  the server. Default is `"new.predictions"`.

- traindataname:

  A character string specifying the name of the dataset used for model
  training.

- na.action:

  A character string to specify the action to take if missing values are
  present. Default is `"na.pass"`.

- datasources:

  A list of
  [`DSConnection-class`](https://datashield.github.io/DSI/reference/DSConnection-class.html)
  objects obtained after login. If the `datasources` argument is not
  specified the default set of connections will be used: see
  [`datashield.connections_default`](https://datashield.github.io/DSI/reference/datashield.connections_default.html).

## Details

This function takes the client-side output from `ds.glm` and sends the
necessary components (coefficients, family, formula, and any categorical
variables) to the server for prediction.

Server function called: `predictDS2`

## Author

Zulal Bekerecioglu

## Examples
