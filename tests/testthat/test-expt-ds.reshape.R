source("connection_to_datasets/init_all_datasets.R")
source("definition_tests/def-ds.reshape.R")
source("definition_tests/def-assign-stats.R")


context("ds.reshape()::expt::multiple")
test_that("copy and transform", 
{
   init.all.datasets()
  .test.reshape()
  

})