source("connection_to_datasets/init_all_datasets.R")
source("definition_tests/def-ds.rUnif.R")

context("ds.rUnif()::expt::no_seeds::single")
test_that("basic expectation",
{
  connect.dataset.2()
  
  .test.basic.expectation(-1,'uniform_dist_1')
  .test.basic.expectation(0,'uniform_dist_2')
  .test.basic.expectation(1,'uniform_dist_3')
  .test.basic.expectation(20,'uniform_dist_4')
  .test.basic.expectation(30,'uniform_dist_5')
  .test.basic.expectation(2^31-1,'uniform_dist_6')
})

context("ds.rUnif()::expt::no_seeds::multiple")
test_that("basic expectation",
{
  connect.all.datasets()
  
  .test.basic.expectation(-1,'uniform_dist_1')
  .test.basic.expectation(0,'uniform_dist_2')
  .test.basic.expectation(1,'uniform_dist_3')
  .test.basic.expectation(20,'uniform_dist_4')
  .test.basic.expectation(30,'uniform_dist_5')
  .test.basic.expectation(2^31-1,'uniform_dist_6')
})

context("ds.rUnif()::expt::seeds::single")
test_that("basic expectation",
{
  connect.dataset.2()
  
  .test.basic.expectation.with.seeds(-1,'uniform_dist_1')
  .test.basic.expectation.with.seeds(0,'uniform_dist_2')
  .test.basic.expectation.with.seeds(1,'uniform_dist_3')
  .test.basic.expectation.with.seeds(20,'uniform_dist_4')
  .test.basic.expectation.with.seeds(30,'uniform_dist_5')
  .test.basic.expectation.with.seeds(2^31-1,'uniform_dist_6')
})

context("ds.rUnif()::expt::seeds::multiple")
test_that("basic expectation",
{
  connect.all.datasets()
  .test.too.large.seed(3)
  .test.too.negative.seed()
  .test.basic.expectation.with.seeds(-1,'uniform_dist_1')
  .test.basic.expectation.with.seeds(0,'uniform_dist_2')
  .test.basic.expectation.with.seeds(1,'uniform_dist_3')
  .test.basic.expectation.with.seeds(20,'uniform_dist_4')
  .test.basic.expectation.with.seeds(30,'uniform_dist_5')
  .test.basic.expectation.with.seeds(2^31-1,'uniform_dist_6')
  
})

