source("connection_to_datasets/init_testing_datasets.R")
source("definition_tests/def-ds.reshape.R")
source("definition_tests/def-assign-stats.R")


context("ds.reshape()::expt::multiple")
test_that("copy and transform", 
{
   init.testing.datasets()
  .test.reshape()
  

})
