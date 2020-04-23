source("definition_tests/def-assign-stats.R")

.test.basic.expectation <- function(size,variable.created)
{
   if (size <= 0)
   {
     expect_error(ds.rPois(samp.size = size,newobj=variable.created))
   }
   else if (size > (2^31-1))
   {
     expect_error(ds.rPois(samp.size = size,newobj=variable.created))
   }
   else
   {
   
      ds.rPois(samp.size = size,newobj=variable.created)
      exists <- ds.exists(variable.created)
      type <- ds.class(variable.created)
      sample.size <- ds.length(variable.created, type = "split")

     
      for(index in 1:length(exists))
      {
        expect_true(exists[[index]])
      }
   
      for(index in 1:length(sample.size))
      {
        expect_true(sample.size[[index]] == size)
      }
   
      expect_true(type[[1]][1]=="integer")
   }
}

.test.basic.expectation.with.seeds <- function(size,variable.created)
{
  seed <- as.integer(as.POSIXct(Sys.time(), "GMT"))/1000
 
  if (size <= 0)
  {
    expect_error(ds.rPois(samp.size = size, 
                          newobj=variable.created,
                          seed.as.integer = seed,
                          return.full.seed.as.set = FALSE))
  }
  else if (size > (2^31-1))
  {
    expect_error(ds.rPois(samp.size = size,
                          newobj=variable.created, 
                          seed.as.integer = seed,
                          return.full.seed.as.set = FALSE))
  }
  
  else
  {
  
    ds.rPois(samp.size = size, 
             newobj=variable.created,
             seed.as.integer = seed,
             return.full.seed.as.set = FALSE)
    exists <- ds.exists(variable.created)
    type <- ds.class(variable.created)
    sample.size <- ds.length(variable.created, type = "split")
    
    for(index in 1:length(exists))
    {
      expect_true(exists[[index]])
    }
    
    for(index in 1:length(sample.size))
    {
      expect_true(sample.size[[index]] == size)
    }
    
    expect_true(type[[1]][1]=="integer")
  }
 
}

.test.too.large.seed <- function(no.server)
{
  seed <- .Machine$integer.max
  if (seed > seed/(no.server +1))
  {
    expect_error(ds.rPois(samp.size = size,
                          newobj=variable.created, 
                          seed.as.integer = seed,
                          return.full.seed.as.set = FALSE))
  }
}

.test.too.negative.seed <- function()
{
  seed <- -100
 
  expect_warning(ds.rPois(samp.size = size,
                          newobj=variable.created, 
                          seed.as.integer = seed,
                          return.full.seed.as.set = FALSE))
  
}


.test.dispersions.stats.same.distribution <- function(seed.first.dist,lambda.first.dist, seed.second.dist, lambda.second.dist)
{
  size <- 20000
  #create distribution on the server
  ds.rPois(samp.size = size, lambda = lambda.first.dist, newobj="first.dist",seed.as.integer = seed.first.dist)
  ds.rPois(samp.size = size, lambda = lambda.second.dist, newobj="second.dist",seed.as.integer = seed.second.dist)
  
  errors <- .compute.errors.between.distributions("first.dist","second.dist",size)
 
  #test
  expect_equal(errors[ds.test_env$MEAN],0, tolerance = ds.test_env$tolerance)
  expect_equal(errors[ds.test_env$VARIANCE],0, tolerance = ds.test_env$tolerance)
}

.test.dispersions.stats.diff.distribution <- function(seed.first.dist,lambda.first.dist, seed.second.dist, lambda.second.dist)
{
  
  size <- 20000
  
  #create distribution on the server
  ds.rPois(samp.size = size, lambda = lambda.first.dist, newobj="first.dist.diff",seed.as.integer = seed.first.dist)
  ds.rPois(samp.size = size, lambda = lambda.second.dist, newobj="second.dist.diff",seed.as.integer = seed.second.dist)
  
  # compute errors
  errors <- .compute.errors.between.distributions("first.dist.diff","second.dist.diff",size)
  
  # test 
  expect_equal(errors[ds.test_env$MEAN],lambda.first.dist - lambda.second.dist, tolerance = 10^1)
  expect_equal(errors[ds.test_env$VARIANCE],lambda.first.dist - lambda.second.dist, tolerance = 10^1)
  

}

.test.lambda.mean.var <- function(seed, lambda.vector)
{
  size <- 20000
  ds.rPois(samp.size = size, lambda = lambda.vector, newobj="first.dist",seed.as.integer = seed)
  first.dist <- .calc.distribution.server("first.dist")
  expect_equal(first.dist[1],lambda.vector[1],tolerance = 10^1)
  expect_equal(first.dist[2],lambda.vector[1],tolerance = 10^1)
}
