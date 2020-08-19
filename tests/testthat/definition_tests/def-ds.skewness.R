

.test.skewness.combined <- function(variable.name,some.values,method)
{
 skewness.local <- e1071::skewness(x = some.values, na.rm = TRUE, type = method) 
 
 skewness.server <- ds.skewness(x = variable.name, method = method, type = "combine")
 skewness.server <- round(as.numeric(skewness.server[[1]][1]), digits = 8)
 
 expect_equal(skewness.server, skewness.local, tolerance = ds.test_env$tolerance)
}

.test.skewness.split <- function(variable.name,some.values.1,some.values.2,some.values.3,method)
{
  skewness.local.1 <- e1071::skewness(x = some.values.1, na.rm = TRUE, type = method)
  skewness.local.2 <- e1071::skewness(x = some.values.2, na.rm = TRUE, type = method)
  skewness.local.3 <- e1071::skewness(x = some.values.3, na.rm = TRUE, type = method)
  
  
  skewness.server <- ds.skewness(x = variable.name, method = method, type = "split")
  
  skewness.server.1 <- round(as.numeric(skewness.server[[1]][1]), digits = 8)
  skewness.server.2 <- round(as.numeric(skewness.server[[1]][2]), digits = 8)
  skewness.server.3 <- round(as.numeric(skewness.server[[1]][3]), digits = 8)
  
  expect_equal(skewness.server.1, skewness.local.1, tolerance = ds.test_env$tolerance)
  expect_equal(skewness.server.2, skewness.local.2, tolerance = ds.test_env$tolerance)
  expect_equal(skewness.server.3, skewness.local.3, tolerance = ds.test_env$tolerance)
}

