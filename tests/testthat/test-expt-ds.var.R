source("connection_to_datasets/init_all_datasets.R")
source("definition_tests/def-ds.var.R")


context("ds.var()::expt::multiple")
test_that("combined data set",
{
  connect.all.datasets()
  .test.var.combined('D$INTEGER',ds.test_env$local.values[,6])
  .test.var.combined('D$NON_NEGATIVE_INTEGER',ds.test_env$local.values[,7])
  .test.var.combined('D$POSITIVE_INTEGER',ds.test_env$local.values[,8])
  .test.var.combined('D$NEGATIVE_INTEGER',ds.test_env$local.values[,9]) 
  .test.var.combined('D$NUMERIC',ds.test_env$local.values[,10])
  .test.var.combined('D$NON_NEGATIVE_NUMERIC',ds.test_env$local.values[,11])
  .test.var.combined('D$POSITIVE_NUMERIC',ds.test_env$local.values[,12])
  .test.var.combined('D$NEGATIVE_NUMERIC',ds.test_env$local.values[,13]) 
})

context("ds.var()::expt::single")
test_that("split data set",
{
  connect.all.datasets()
  .test.var.split('D$INTEGER',ds.test_env$local.values.1[,6],ds.test_env$local.values.2[,6],ds.test_env$local.values.3[,6])
  .test.var.split('D$NON_NEGATIVE_INTEGER',ds.test_env$local.values.1[,7],ds.test_env$local.values.2[,7],ds.test_env$local.values.3[,7])
  .test.var.split('D$POSITIVE_INTEGER',ds.test_env$local.values.1[,8],ds.test_env$local.values.2[,8],ds.test_env$local.values.3[,8])
  .test.var.split('D$NEGATIVE_INTEGER',ds.test_env$local.values.1[,9],ds.test_env$local.values.2[,9],ds.test_env$local.values.3[,9])
  .test.var.split('D$NUMERIC',ds.test_env$local.values.1[,10],ds.test_env$local.values.2[,10],ds.test_env$local.values.3[,10])
  .test.var.split('D$NON_NEGATIVE_NUMERIC',ds.test_env$local.values.1[,11],ds.test_env$local.values.2[,11],ds.test_env$local.values.3[,11])
  .test.var.split('D$POSITIVE_NUMERIC',ds.test_env$local.values.1[,12],ds.test_env$local.values.2[,12],ds.test_env$local.values.3[,12])
  .test.var.split('D$NEGATIVE_NUMERIC',ds.test_env$local.values.1[,13],ds.test_env$local.values.2[,13],ds.test_env$local.values.3[,13])
})

context("ds.var()::expt::large_values::multiple")
test_that("large values",
{
  connect.all.datasets()
  .test.variance.large('D$INTEGER',ds.test_env$local.values[,6])
  .test.variance.large('D$NON_NEGATIVE_INTEGER',ds.test_env$local.values[,7])
  .test.variance.large('D$POSITIVE_INTEGER',ds.test_env$local.values[,8])
  .test.variance.large('D$NEGATIVE_INTEGER',ds.test_env$local.values[,9]) 
  .test.variance.large('D$NUMERIC',ds.test_env$local.values[,10])
  .test.variance.large('D$NON_NEGATIVE_NUMERIC',ds.test_env$local.values[,11])
  .test.variance.large('D$POSITIVE_NUMERIC',ds.test_env$local.values[,12])
  .test.variance.large('D$NEGATIVE_NUMERIC',ds.test_env$local.values[,13]) 
})

context("ds.var()::expt::large_values::single")
test_that("large values",
{
  connect.dataset.1()
  .test.variance.large('D$INTEGER',ds.test_env$local.values.1[,6])
  .test.variance.large('D$NON_NEGATIVE_INTEGER',ds.test_env$local.values.1[,7])
  .test.variance.large('D$POSITIVE_INTEGER',ds.test_env$local.values.1[,8])
  .test.variance.large('D$NEGATIVE_INTEGER',ds.test_env$local.values.1[,9])
  .test.variance.large('D$NUMERIC',ds.test_env$local.values.1[,10])
  .test.variance.large('D$NON_NEGATIVE_NUMERIC',ds.test_env$local.values.1[,11])
  .test.variance.large('D$POSITIVE_NUMERIC',ds.test_env$local.values.1[,12])
  .test.variance.large('D$NEGATIVE_NUMERIC',ds.test_env$local.values.1[,13]) 
})


