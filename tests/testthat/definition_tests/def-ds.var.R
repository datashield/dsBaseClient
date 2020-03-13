source("connection_to_datasets/init_testing_datasets.R")
source("definition_tests/def-assign-stats.R")

.test.var.combined <- function(variable.name,some.values)
{
  var.local <- var(some.values)
  var.server <- ds.var(variable.name,type='combine', check=TRUE)
  expect_equal(var.server[[1]][1], var.local, tolerance = ds.test_env$tolerance)
}

.test.var.split <- function(variable.name,some.values.1,some.values.2,some.values.3)
{
  var.local.1 <- var(some.values.1)
  var.local.2 <- var(some.values.2)
  var.local.3 <- var(some.values.3)
  
  
  var.servers <- ds.var(x=variable.name,type='split', check=TRUE)
  expect_equal(var.servers[[1]][1], var.local.1, tolerance = ds.test_env$tolerance)
  expect_equal(var.servers[[1]][2], var.local.2, tolerance = ds.test_env$tolerance)
  expect_equal(var.servers[[1]][3], var.local.3, tolerance = ds.test_env$tolerance)
}

.test.variance.positive.combine <- function(variable.name)
{
  var.server <- ds.var(variable.name,type='combine', check=TRUE)
  expect_true(var.server$Global.Variance[1] >= 0)
}

.test.variance.positive.split <- function(variable.name)
{
  var.server <- ds.var(variable.name,type='split', check=TRUE)
  expect_true(var.server[[1]][1] >= 0)
  expect_true(var.server[[1]][2] >= 0)
  expect_true(var.server[[1]][3] >= 0)
}

.test.standard.dev.combine <- function(variable.name, some.values)
{
  std.local <- sd(some.values)
  var.server <- ds.var(variable.name,type='combine')
  std.server <- sqrt(var.server$Global.Variance[1])
  expect_equal(std.server,std.local, tolerance = ds.test_env$tolerance)
}

.test.standard.dev.split <- function(variable.name, some.values.1,some.values.2,some.values.3)
{
  std.local.1 <- sd(some.values.1)
  std.local.2 <- sd(some.values.2)
  std.local.3 <- sd(some.values.3)
  
  var.servers <- ds.var(variable.name,type='split')
  std.servers <- c()
  std.servers[1] <- sqrt(var.servers[[1]][1])
  std.servers[2] <- sqrt(var.servers[[1]][2])
  std.servers[3] <- sqrt(var.servers[[1]][3])
  
  expect_equal(std.servers[1],std.local.1, tolerance = ds.test_env$tolerance)
  expect_equal(std.servers[2],std.local.2, tolerance = ds.test_env$tolerance)
  expect_equal(std.servers[3],std.local.3, tolerance = ds.test_env$tolerance)
}

.test.variance.large <- function(variable.name, some.values)
{
  
  #local values 
  large.local.values <- .mult.vectors(some.values, some.values)
  var.local <- var(large.local.values)
  
  #server values
  variable.created <- "temp.value"
  operation <- paste("(",variable.name, "*",variable.name,")",sep="")
  ds.make(operation,variable.created)

  
  var.server <- ds.var(variable.created,type='combine', check=TRUE)
  expect_equal(var.server[[1]][1], var.local, tolerance = ds.test_env$tolerance)
}

.test.location.parameter <- function(variable.name)
{
  #define a constant values
  constant.value <- sample(-1000:1000,1)
  variable.created <- "temp.value"
  operation <- paste("(",variable.name, "+",constant.value,")",sep="")
  ds.make(operation,variable.created)
  
  #calculate variances
  var.no.change <- ds.var(variable.name,type='combine', check=TRUE)
  var.changes <- ds.var(variable.created,type='combine', check=TRUE)
  
  #comparison
  expect_equal(var.no.change[[1]][1], var.changes[[1]][1], tolerance = ds.test_env$tolerance)
}

.test.scale <- function(variable.name)
{
  #define a constant values
  constant.value <- sample(-10:10,1)
  variable.created <- "temp.value"
  operation <- paste("(",variable.name,"*",constant.value,")",sep="")
  ds.make(operation,variable.created)
  
  #calculate variances
  var.no.change <- ds.var(variable.name,type='combine', check=TRUE)
  var.changes <- ds.var(variable.created,type='combine', check=TRUE)
  scale <- constant.value^2 * var.no.change[[1]][1]
  #comparison
  expect_equal(scale, var.changes[[1]][1], tolerance = ds.test_env$tolerance)
}

.test.large.vectors  <- function(dist.name, size)
{
  ds.rPois(samp.size = size, lambda = c(50), newobj=dist.name)
  var.changes <- ds.var(dist.name,type='combine', check=TRUE)
  expect_false(is.na(var.changes[[1]][1]))
}
