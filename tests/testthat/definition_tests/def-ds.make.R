source("connection_to_datasets/init_testing_datasets.R")
source("definition_tests/def-assign-stats.R")

.test.copy.data <-function(variable.name, variable.created, some.values)
{
  #calculate the distribution of the localserver
  dist.local <- .calc.distribution.locally(some.values)
  dist.server.original <- .calc.distribution.server(variable.name)
  
  #create a new object with the same values. calculate distribution of new object
  ds.make(variable.name,variable.created)
  dist.server.new.object <- .calc.distribution.server(variable.created)
 
  #compare the results between the local and server data distribution.
  expect_equal(dist.local[1],dist.server.original[1], tolerance = ds.test_env$tolerance)
  expect_equal(dist.local[2],dist.server.original[2], tolerance = ds.test_env$tolerance)
  expect_equal(dist.local[1],dist.server.new.object[1], tolerance = ds.test_env$tolerance)
  expect_equal(dist.local[2],dist.server.new.object[2], tolerance = ds.test_env$tolerance)
  expect_equal(dist.server.original[1],dist.server.new.object[1], tolerance = ds.test_env$tolerance)
  expect_equal(dist.server.original[2],dist.server.new.object[2], tolerance = ds.test_env$tolerance)
}

.test.operation.vectors <- function(first.variable.name, second.variable.name, variable.created, arithmetic.operator, result.local)
{
  #build expressions and applies on the server
  operation <- paste("(",first.variable.name, arithmetic.operator, second.variable.name,")",sep="")
  ds.make(operation,variable.created)
  
  #distribution the results between the local and server data.
  dist.local <- .calc.distribution.locally(result.local)
  dist.server.new.object <- .calc.distribution.server(variable.created)

 
  #compare the results between the local and server data distribution.
  expect_equal(dist.local[1],dist.server.new.object[1], tolerance = ds.test_env$tolerance)
  expect_equal(dist.local[2],dist.server.new.object[2], tolerance = ds.test_env$tolerance)
}

.test.operation.constant <- function(first.variable.name, constant.value, variable.created, arithmetic.operator, result.local)
{
  #build expressions and applies on the server
  operation <- paste("(",first.variable.name, arithmetic.operator, constant.value,")",sep="")
  ds.make(operation,variable.created)
  
  #distribution the results between the local and server data.
  dist.local <- .calc.distribution.locally(result.local)
  dist.server.new.object <- .calc.distribution.server(variable.created)
  
  #compare the results between the local and server data distribution.
  expect_equal(dist.local[1],dist.server.new.object[1], tolerance = ds.test_env$tolerance)
  expect_equal(dist.local[2],dist.server.new.object[2], tolerance = ds.test_env$tolerance)
}

.test.operation.mean <- function(first.variable.name, constant.value, variable.created, arithmetic.operator)
{
   #calc mean of the original values
   server.dist <- .calc.distribution.server(first.variable.name)
   mean.original <- server.dist[1]

   #build expressions and applies on the server
   operation <- paste("(",first.variable.name, arithmetic.operator, constant.value,")",sep="")
   result.server <- ds.make(operation,variable.created)
   
   #distribution the results server data.
   dist.server.new.object <- .calc.distribution.server(variable.created)
  
   #transform the mean by applying the same operations and constant value to the mean. 
   expression <- paste("mean.transformed <- mean.original", arithmetic.operator, constant.value)
   eval(parse(text = expression))
   
   #compare results
   expect_equal(mean.transformed,dist.server.new.object[1])
   expect_true(mean.original != mean.transformed)
}

.test.copy.apply.operator <- function(first.variable.name, constant.value, variable.created, arithmetic.operator, some.values,result.local)
{
  #calculate the distribution of the localserver
  dist.local <- .calc.distribution.locally(some.values)
  dist.local.result <- .calc.distribution.locally(result.local)
  dist.server.original <- .calc.distribution.server(first.variable.name)
  
  #create a new object with the same values. calculate distribution of new object
  variable.created.cp <- paste(variable.created, "_cp", sep="")
  ds.make(first.variable.name,variable.created.cp)
  dist.server.new.object <- .calc.distribution.server(variable.created)
  
  #apply the operator to the server
  operation <- paste("(",variable.created.cp, arithmetic.operator, constant.value,")",sep="")
  ds.make(operation,variable.created)
  
  #distribution second changes
  dist.server.result <- .calc.distribution.server(variable.created)
  
  #compare the results between the local and server data distribution.
  expect_equal(dist.local[1],dist.server.original[1], tolerance = ds.test_env$tolerance)
  expect_equal(dist.local[2],dist.server.original[2], tolerance = ds.test_env$tolerance)
  expect_equal(dist.local[1],dist.server.new.object[1], tolerance = ds.test_env$tolerance)
  expect_equal(dist.local[2],dist.server.new.object[2], tolerance = ds.test_env$tolerance)
  expect_equal(dist.server.original[1],dist.server.new.object[1], tolerance = ds.test_env$tolerance)
  expect_equal(dist.server.original[2],dist.server.new.object[2], tolerance = ds.test_env$tolerance)
  expect_equal(dist.local.result[1],dist.server.result[1], tolerance = ds.test_env$tolerance)
  expect_equal(dist.local.result[2],dist.server.result[2], tolerance = ds.test_env$tolerance)
  
}

.test.inverse <- function(first.variable.name, constant.value, variable.created, arithmetic.operator, inverse.operator)
{
  #calc mean of the original values
  dist.server <- .calc.distribution.server(first.variable.name)

  #build expressions and applies on the server
  operation <- paste("(",first.variable.name, arithmetic.operator, constant.value,")",sep="")
  result.server <- ds.make(operation,variable.created)
  
  #distribution the results server data.
  dist.server.prime <- .calc.distribution.server(variable.created)
  
  #build expressions and applies on the server - inverse
  operation <- paste("(", variable.created, inverse.operator, constant.value,")",sep="")
  variable.created.inverse <- paste(variable.created, ".inv",sep="")
  result.server.inverse <- ds.make(operation,variable.created.inverse)
  #print(result.server.inverse)
  
  
  #distribution the inverse set
  dist.server.inverse <- .calc.distribution.server(variable.created.inverse)
  
  #compare results
  expect_equal(dist.server[1], dist.server.inverse[1])
  expect_equal(dist.servert[2], dist.server.inverse[2])
  expect_true(dist.server[1] != dist.server.prime[1])
  expect_true(dist.server.prime[1] != dist.server.inverse[1])
}
