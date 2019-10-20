source("definition_tests/def-assign-stats.R")



#check sample of reasonable size are created. Also verifies a variable
#on the server is created with a suitable type (i.e. integer).
#The length of each sample on each server is checked.
#No seed is used
.test.basic.expectation <- function(size,variable.created)
{
  if (size <= 0)
  { 
    expect_error(ds.rUnif(samp.size = size, newobj = variable.created))
  } 
  else if (size > (2^31-1))
  {
    expect_error(ds.rUnif(samp.size = size, newobj = variable.created))
  }
  else
  {
    ds.rUnif(samp.size = size, newobj = variable.created)
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

#check sample of reasonable size are created. Also verifies a variable
#on the server is created with a suitable type (i.e. integer).
#The length of each sample on each server is checked.
#Some randomly generated seeds are used.
.test.basic.expectation.with.seeds <- function(size,variable.created)
{
  seed <- as.integer(as.POSIXct(Sys.time(), "GMT"))/1000
  
  if (size <= 0)
  {
    expect_error(ds.rUnif(samp.size = size, newobj = variable.created, seed.as.integer = seed))
  }
  else if (size > (2^31-1))
  {
    expect_error(ds.rUnif(samp.size = size, newobj = variable.created, seed.as.integer = seed))
  }
  else
  {
    
    ds.rUnif(samp.size = size, newobj = variable.created, seed.as.integer = seed)
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

#test whether a distribution with a large seed can be created (i.e. maximum size of a sample)
.test.too.large.seed <- function(no.server)
{
  seed <- .Machine$integer.max
  if (seed > seed/(no.server +1))
  {
    expect_error(ds.rUnif(samp.size = 100, newobj = "dist.created", seed.as.integer = seed))
  }
}

.test.too.negative.seed <- function()
{
  seed <- -100
  
  expect_warning(ds.rUnif(samp.size = 100, newobj = "dist.created", seed.as.integer = seed))
  
}



.test.range.values <- function(min.value, max.value, variable.created, seed)
{
  # init size of sample and randomly generates distribution on server
  size <- 20000
  ds.rUnif(samp.size = size, min = min.value, max = max.value, newobj = variable.created, seed.as.integer = seed)
 
  # compute distribution statistics
  dist.stats        <- .calc.distribution.server(variable.created)
  expected.mean     <-  (1/2)*(min.value + max.value)
  expected.variance <- (1/12)*(max.value - min.value)^2

  
  # test 
  expect_equal(dist.stats[5],max.value, tolerance = 10^2)
  expect_equal(dist.stats[4],min.value, tolerance = 10^2)
  expect_equal(dist.stats[1], expected.mean, tolerance = 10^2) 
  expect_equal(dist.stats[2], expected.variance, tolerance = 10^2) 
}


.test.dispersions.stats.same.distribution <- function(min.value, max.value, seed)
{
  size <- 20000
  #create distribution on the server
  ds.rUnif(samp.size = size, min = min.value, max = max.value, newobj ="first.dist",  seed.as.integer = seed)
  ds.rUnif(samp.size = size, min = min.value, max = max.value, newobj ="second.dist", seed.as.integer = seed)
  
  #compute errors 
  errors <- .compute.errors.between.distributions("first.dist","second.dist",size)
  
  #test
  expect_equal(errors[1],0, tolerance = ds.test_env$tolerance)
  expect_equal(errors[2],0, tolerance = ds.test_env$tolerance)
}

.test.dispersions.stats.diff.distribution <- function(first.dist.min.value, first.dist.max.value, first.seed, second.dist.min.value, second.dist.max.value, second.seed)
{
  #initialisation of variables
  size <- 20000
  expected.errors <- c(0,0)
  
  #create distribution on the server
  ds.rUnif(samp.size = size, min = first.dist.min.value, max = first.dist.max.value, newobj ="first.dist", seed.as.integer = first.seed)
  ds.rUnif(samp.size = size, min = second.dist.min.value, max = second.dist.max.value, newobj ="second.dist", seed.as.integer = second.seed)

  #compute errors 
  errors <- .compute.errors.between.distributions("first.dist","second.dist",size)
  expected.errors[1] <- (((1/2)*(first.dist.max.value - first.dist.min.value)) - 
                               ((1/2)*(second.dist.max.value - second.dist.min.value)))/size
  
  expected.errors[2] <- (((1/12)*(first.dist.max.value - first.dist.min.value)^2) - 
                               ((1/12)*(second.dist.max.value - second.dist.min.value)^2))/size
  
  #test
  expect_equal(errors[1],expected.errors[1], tolerance = 10^2)
  expect_equal(errors[2],expected.errors[2], tolerance = 10^2)
  
}



