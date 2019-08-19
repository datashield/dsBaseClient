source("connection_to_datasets/init_all_datasets.R")
source("definition_tests/def-ds.mean.R")


context("ds.mean()::expt::combine::multiple")
test_that("combined data set",
{
  connect.all.datasets()
  .test.mean.combined('D$INTEGER',ds.test_env$local.values[,6])
  .test.mean.combined('D$NON_NEGATIVE_INTEGER',ds.test_env$local.values[,7])
  .test.mean.combined('D$POSITIVE_INTEGER',ds.test_env$local.values[,8])
  .test.mean.combined('D$NEGATIVE_INTEGER',ds.test_env$local.values[,9]) 
  .test.mean.combined('D$NUMERIC',ds.test_env$local.values[,10])
  .test.mean.combined('D$NON_NEGATIVE_NUMERIC',ds.test_env$local.values[,11])
  .test.mean.combined('D$POSITIVE_NUMERIC',ds.test_env$local.values[,12])
  .test.mean.combined('D$NEGATIVE_NUMERIC',ds.test_env$local.values[,13]) 
})

context("ds.mean()::expt::split::multiple")
test_that("split data set",
{
  connect.all.datasets()
  .test.mean.split('D$INTEGER',ds.test_env$local.values.1[,6],ds.test_env$local.values.2[,6],ds.test_env$local.values.3[,6])

  .test.mean.split('D$NON_NEGATIVE_INTEGER',ds.test_env$local.values.1[,7],ds.test_env$local.values.2[,7],ds.test_env$local.values.3[,7])
  .test.mean.split('D$POSITIVE_INTEGER',ds.test_env$local.values.1[,8],ds.test_env$local.values.2[,8],ds.test_env$local.values.3[,8])
  .test.mean.split('D$NEGATIVE_INTEGER',ds.test_env$local.values.1[,9],ds.test_env$local.values.2[,9],ds.test_env$local.values.3[,9])
  .test.mean.split('D$NUMERIC',ds.test_env$local.values.1[,10],ds.test_env$local.values.2[,10],ds.test_env$local.values.3[,10])
  .test.mean.split('D$NON_NEGATIVE_NUMERIC',ds.test_env$local.values.1[,11],ds.test_env$local.values.2[,11],ds.test_env$local.values.3[,11])
  .test.mean.split('D$POSITIVE_NUMERIC',ds.test_env$local.values.1[,12],ds.test_env$local.values.2[,12],ds.test_env$local.values.3[,12])
  .test.mean.split('D$NEGATIVE_NUMERIC',ds.test_env$local.values.1[,13],ds.test_env$local.values.2[,13],ds.test_env$local.values.3[,13])
})

context("ds.mean()::expt::single")
test_that("combined data set",
{
  connect.dataset.1()
  .test.mean.combined('D$INTEGER',ds.test_env$local.values.1[,6])
  .test.mean.combined('D$NON_NEGATIVE_INTEGER',ds.test_env$local.values.1[,7])
  .test.mean.combined('D$POSITIVE_INTEGER',ds.test_env$local.values.1[,8])
  .test.mean.combined('D$NEGATIVE_INTEGER',ds.test_env$local.values.1[,9]) 
  .test.mean.combined('D$NUMERIC',ds.test_env$local.values.1[,10])
  .test.mean.combined('D$NON_NEGATIVE_NUMERIC',ds.test_env$local.values.1[,11])
  .test.mean.combined('D$POSITIVE_NUMERIC',ds.test_env$local.values.1[,12])
  .test.mean.combined('D$NEGATIVE_NUMERIC',ds.test_env$local.values.1[,13]) 
})

context("ds.mean()::expt::large_values::single")
test_that("combined data set",
{
  connect.dataset.1()
  .test.mean.large('D$INTEGER',ds.test_env$local.values.1[,6])
  .test.mean.large('D$NON_NEGATIVE_INTEGER',ds.test_env$local.values.1[,7])
  .test.mean.large('D$POSITIVE_INTEGER',ds.test_env$local.values.1[,8])
  .test.mean.large('D$NEGATIVE_INTEGER',ds.test_env$local.values.1[,9]) 
  .test.mean.large('D$NUMERIC',ds.test_env$local.values.1[,10])
  .test.mean.large('D$NON_NEGATIVE_NUMERIC',ds.test_env$local.values.1[,11])
  .test.mean.large('D$POSITIVE_NUMERIC',ds.test_env$local.values.1[,12])
  .test.mean.large('D$NEGATIVE_NUMERIC',ds.test_env$local.values.1[,13]) 
})

context("ds.mean()::expt::large_values::multiple")
test_that("combined data set",
{
  connect.all.datasets()
  .test.mean.large('D$INTEGER',ds.test_env$local.values[,6])
  .test.mean.large('D$NON_NEGATIVE_INTEGER',ds.test_env$local.values[,7])
  .test.mean.large('D$POSITIVE_INTEGER',ds.test_env$local.values[,8])
  .test.mean.large('D$NEGATIVE_INTEGER',ds.test_env$local.values[,9]) 
  .test.mean.large('D$NUMERIC',ds.test_env$local.values[,10])
  .test.mean.large('D$NON_NEGATIVE_NUMERIC',ds.test_env$local.values[,11])
  .test.mean.large('D$POSITIVE_NUMERIC',ds.test_env$local.values[,12])
  .test.mean.large('D$NEGATIVE_NUMERIC',ds.test_env$local.values[,13]) 
})

