
source("definition_tests/def-assign-stats.R")

.test.mean.combined <- function(variable.name,some.values)
{
  mean.local <- mean(some.values)
  mean.server <- ds.mean(x=variable.name,type='combine', check=TRUE,save.mean.Nvalid=FALSE)

  expect_equal(mean.server[[1]][1], mean.local, tolerance = ds.test_env$tolerance)
}

.test.mean.split <- function(variable.name,some.values.1,some.values.2,some.values.3)
{
  mean.local.1 <- mean(some.values.1)
  mean.local.2 <- mean(some.values.2)
  mean.local.3 <- mean(some.values.3)
  
  mean.server <- ds.mean(x=variable.name,type='split', check=TRUE,save.mean.Nvalid=FALSE)
  expect_equal(mean.server[[1]][1], mean.local.1, tolerance = ds.test_env$tolerance)
  expect_equal(mean.server[[1]][2], mean.local.2, tolerance = ds.test_env$tolerance)
  expect_equal(mean.server[[1]][3], mean.local.3, tolerance = ds.test_env$tolerance)
}

.test.residual.combined <- function(variable.name, some.values)
{
  mean.server <- ds.mean(variable.name,type='combine', check=TRUE,save.mean.Nvalid=FALSE)
  residue <- sum(some.values - mean.server[[1]][1])
  expect_equal(residue, 0, tolerance = ds.test_env$tolerance)
}  


.test.residual.split <- function(variable.name, some.values.1,some.values.2,some.values.3)
{
  mean.server <- ds.mean(variable.name,type='split', check=TRUE,save.mean.Nvalid=FALSE)
  residue.1 <- sum(some.values.1 - mean.server[[1]][1])
  residue.2 <- sum(some.values.2 - mean.server[[1]][2])
  residue.3 <- sum(some.values.3 - mean.server[[1]][3])
  
  expect_equal(residue.1, 0, tolerance = ds.test_env$tolerance)
  expect_equal(residue.2, 0, tolerance = ds.test_env$tolerance)
  expect_equal(residue.3, 0, tolerance = ds.test_env$tolerance)
}

.test.mean.large <- function(variable.name, some.values)
{
  
  #local values 
  large.local.values <- .mult.vectors(some.values, some.values)
  mean.local <- mean(large.local.values)
  
  #server values
  variable.created <- "temp.value"
  operation <- paste("(",variable.name, "*",variable.name,")",sep="")
  ds.make(operation,variable.created)
  
  
  mean.server <- ds.mean(variable.created,type='combine', check=TRUE)
  expect_equal(mean.server[[1]][1], mean.local, tolerance = ds.test_env$tolerance)
}

.test.location.parameter <- function(variable.name)
{
  #define a constant values
  constant.value <- sample(-1000:1000,1)
  variable.created <- "temp.value"
  operation <- paste("(",variable.name, "+",constant.value,")",sep="")
  ds.make(operation,variable.created)
  
  #calculate variances
  var.no.change <- ds.mean(variable.name,type='combine',check=TRUE,save.mean.Nvalid=FALSE)
  var.changes <- ds.mean(variable.created,type='combine', check=TRUE,save.mean.Nvalid=FALSE)
  difference <- var.changes[[1]][1] - var.no.change[[1]][1]
 
  #comparison
  expect_equal(constant.value, difference, tolerance = ds.test_env$tolerance)
}

.test.scale <- function(variable.name)
{
  #define a constant values
  constant.value <- sample(-10:10,1)
  variable.created <- "temp.value"
  operation <- paste("(",variable.name,"*",constant.value,")",sep="")
  ds.make(operation,variable.created)
  
  #calculate variances
  var.no.change <- ds.mean(variable.name,type='combine',check=TRUE,save.mean.Nvalid=FALSE)
  var.changes <- ds.mean(variable.created,type='combine', check=TRUE,save.mean.Nvalid=FALSE)
  scale <- constant.value * var.no.change[[1]][1]
  #comparison
  expect_equal(scale, var.changes[[1]][1], tolerance = ds.test_env$tolerance)
}

