source("connection_to_datasets/init_all_datasets.R")
source("definition_tests/def-ds.rPois.R")

random.number <- as.integer(as.POSIXct(Sys.time(), "GMT"))

context("ds.rPois()::math::lambda equal mean and var::single")
test_that("lamdba == mean and lambda == var",
{
  connect.dataset.1()
  seed <- random.number/1000
  .test.lambda.mean.var(seed, c(1))
  .test.lambda.mean.var(seed, c(2))
  .test.lambda.mean.var(seed, c(3))
  
})

context("ds.rPois()::math::lambda equal mean and var::multiple")
test_that("lamdba == mean and lambda == var",
{
  connect.all.datasets()
  seed <- random.number/1000
  .test.lambda.mean.var(seed, c(1))
  .test.lambda.mean.var(seed, c(2))
  .test.lambda.mean.var(seed, c(3))
  
})

context("ds.rPois()::math::distributions comparison::multiple")
test_that("changes in distribution",
{
  connect.all.datasets()
  seed <- random.number/1000
  .test.dispersions.stats.same.distribution(seed, c(6),seed,c(6))
  .test.dispersions.stats.diff.distribution(seed/1000, c(10),seed,c(8))
  .test.dispersions.stats.diff.distribution(seed/1000, c(19),seed,c(11))
  .test.dispersions.stats.diff.distribution(seed/1000, c(1000),seed,c(8000))
  .test.dispersions.stats.diff.distribution(seed/1000, c(1900),seed,c(1100))
})

context("ds.rPois()::math::distributions comparison::single")
test_that("changes in distribution",
{
  connect.dataset.3()
  seed <- random.number/1000
  .test.dispersions.stats.same.distribution(seed, c(6),seed,c(6))
  .test.dispersions.stats.diff.distribution(seed/1000, c(10),seed,c(8))
  .test.dispersions.stats.diff.distribution(seed/1000, c(19),seed,c(11))
})

