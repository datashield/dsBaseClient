source("connection_to_datasets/init_all_datasets.R")
source("definition_tests/def-ds.rUnif.R")

random.number <- as.integer(as.POSIXct(Sys.time(), "GMT"))

context("ds.rUnif()::math::mean_variance::single")
test_that("mean_variance", 
{
  connect.dataset.1()
  .test.range.values(1,2,"uniform_1")
  .test.range.values(-1,2,"uniform_2")
  .test.range.values(-200,-100,"uniform_3")
})

context("ds.rUnif()::math::distributions comparison::single")
test_that("changes in distribution",
{
  connect.dataset.3()
  seed <- random.number/1000
  .test.dispersions.stats.same.distribution(0,1,seed)
  .test.dispersions.stats.same.distribution(0,100,seed)
  .test.dispersions.stats.same.distribution(50,100,seed)
  .test.dispersions.stats.diff.distribution(0,1,seed,100,200,seed)
  .test.dispersions.stats.diff.distribution(0,1,seed,1,2, seed)
  .test.dispersions.stats.diff.distribution(100,200, seed, 1000,2000, seed)
})


context("ds.rUnif()::math::distributions comparison::multiple")
test_that("changes in distribution",
{
   connect.all.datasets()
   seed <- random.number/1000
   .test.dispersions.stats.same.distribution(0,1,seed)
   .test.dispersions.stats.same.distribution(0,100,seed)
   .test.dispersions.stats.same.distribution(50,100,seed)
   .test.dispersions.stats.diff.distribution(0,1,seed, 100,200,seed)
   .test.dispersions.stats.diff.distribution(0,1,seed, 1,2, seed)
   .test.dispersions.stats.diff.distribution(100,200, seed, 1000,2000, seed)
})

