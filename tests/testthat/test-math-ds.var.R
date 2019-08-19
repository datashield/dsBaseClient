source("connection_to_datasets/init_all_datasets.R")
source("definition_tests/def-ds.var.R")

context("ds.var()::math::positive_result::multiple")
test_that("variance >=0",
{
  connect.all.datasets()
  .test.variance.positive.combine('D$INTEGER')
  .test.variance.positive.combine('D$NON_NEGATIVE_INTEGER')
  .test.variance.positive.combine('D$POSITIVE_INTEGER')
  .test.variance.positive.combine('D$NEGATIVE_INTEGER')
  .test.variance.positive.combine('D$NUMERIC')
  .test.variance.positive.combine('D$NON_NEGATIVE_NUMERIC')
  .test.variance.positive.combine('D$POSITIVE_NUMERIC')
  .test.variance.positive.combine('D$NEGATIVE_NUMERIC')
  
  .test.variance.positive.split('D$INTEGER')
  .test.variance.positive.split('D$NON_NEGATIVE_INTEGER')
  .test.variance.positive.split('D$POSITIVE_INTEGER')
  .test.variance.positive.split('D$NEGATIVE_INTEGER')
  .test.variance.positive.split('D$NUMERIC')
  .test.variance.positive.split('D$NON_NEGATIVE_NUMERIC')
  .test.variance.positive.split('D$POSITIVE_NUMERIC')
  .test.variance.positive.split('D$NEGATIVE_NUMERIC')
})


context("ds.var()::math::positive_result::single")
test_that("variance >=0",
{
  connect.dataset.1()
  .test.variance.positive.combine('D$INTEGER')
  .test.variance.positive.combine('D$NON_NEGATIVE_INTEGER')
  .test.variance.positive.combine('D$POSITIVE_INTEGER')
  .test.variance.positive.combine('D$NEGATIVE_INTEGER')
  .test.variance.positive.combine('D$NUMERIC')
  .test.variance.positive.combine('D$NON_NEGATIVE_NUMERIC')
  .test.variance.positive.combine('D$POSITIVE_NUMERIC')
  .test.variance.positive.combine('D$NEGATIVE_NUMERIC')
  
  .test.variance.positive.split('D$INTEGER')
  .test.variance.positive.split('D$NON_NEGATIVE_INTEGER')
  .test.variance.positive.split('D$POSITIVE_INTEGER')
  .test.variance.positive.split('D$NEGATIVE_INTEGER')
  .test.variance.positive.split('D$NUMERIC')
  .test.variance.positive.split('D$NON_NEGATIVE_NUMERIC')
  .test.variance.positive.split('D$POSITIVE_NUMERIC')
  .test.variance.positive.split('D$NEGATIVE_NUMERIC')
})


context("ds.var()::math::square_root_std::single")
test_that("variance is to the power of 2 of the standard deviation",
{
  connect.dataset.1()
  .test.standard.dev.combine('D$INTEGER',ds.test_env$local.values.1[,6])
  .test.standard.dev.combine('D$NON_NEGATIVE_INTEGER',ds.test_env$local.values.1[,7])
  .test.standard.dev.combine('D$POSITIVE_INTEGER',ds.test_env$local.values.1[,8])
  .test.standard.dev.combine('D$NEGATIVE_INTEGER',ds.test_env$local.values.1[,9])
  .test.standard.dev.combine('D$NUMERIC',ds.test_env$local.values.1[,10])
  .test.standard.dev.combine('D$NON_NEGATIVE_NUMERIC',ds.test_env$local.values.1[,11])
  .test.standard.dev.combine('D$POSITIVE_NUMERIC',ds.test_env$local.values.1[,12])
  .test.standard.dev.combine('D$NEGATIVE_NUMERIC',ds.test_env$local.values.1[,13])
  
})


context("ds.var()::math::square_root_std::multiple")
test_that("variance is to the power of 2 of the standard deviation",
 {
   connect.all.datasets()
   .test.standard.dev.combine('D$INTEGER',ds.test_env$local.values[,6])
   .test.standard.dev.combine('D$NON_NEGATIVE_INTEGER',ds.test_env$local.values[,7])
   .test.standard.dev.combine('D$POSITIVE_INTEGER',ds.test_env$local.values[,8])
   .test.standard.dev.combine('D$NEGATIVE_INTEGER',ds.test_env$local.values[,9])
   .test.standard.dev.combine('D$NUMERIC',ds.test_env$local.values[,10])
   .test.standard.dev.combine('D$NON_NEGATIVE_NUMERIC',ds.test_env$local.values[,11])
   .test.standard.dev.combine('D$POSITIVE_NUMERIC',ds.test_env$local.values[,12])
   .test.standard.dev.combine('D$NEGATIVE_NUMERIC',ds.test_env$local.values[,13])
   
   .test.standard.dev.split('D$INTEGER',ds.test_env$local.values.1[,6],ds.test_env$local.values.2[,6],ds.test_env$local.values.3[,6])
   .test.standard.dev.split('D$NON_NEGATIVE_INTEGER',ds.test_env$local.values.1[,7],ds.test_env$local.values.2[,7],ds.test_env$local.values.3[,7])
   .test.standard.dev.split('D$POSITIVE_INTEGER',ds.test_env$local.values.1[,8],ds.test_env$local.values.2[,8],ds.test_env$local.values.3[,8])
   .test.standard.dev.split('D$NEGATIVE_INTEGER',ds.test_env$local.values.1[,9],ds.test_env$local.values.2[,9],ds.test_env$local.values.3[,9])
   .test.standard.dev.split('D$NUMERIC',ds.test_env$local.values.1[,10],ds.test_env$local.values.2[,10],ds.test_env$local.values.3[,10])
   .test.standard.dev.split('D$NON_NEGATIVE_NUMERIC',ds.test_env$local.values.1[,11],ds.test_env$local.values.2[,11],ds.test_env$local.values.3[,11])
   .test.standard.dev.split('D$POSITIVE_NUMERIC',ds.test_env$local.values.1[,12],ds.test_env$local.values.2[,12],ds.test_env$local.values.3[,12])
   .test.standard.dev.split('D$NEGATIVE_NUMERIC',ds.test_env$local.values.1[,13],ds.test_env$local.values.2[,13],ds.test_env$local.values.3[,13])
 })

context("ds.var()::math::location::parameter::single")
test_that("var (X+a) = Var(X)",
{
  connect.dataset.1()
  .test.location.parameter('D$INTEGER')
  .test.location.parameter('D$NON_NEGATIVE_INTEGER')
  .test.location.parameter('D$POSITIVE_INTEGER')
  .test.location.parameter('D$NEGATIVE_INTEGER')
  .test.location.parameter('D$NUMERIC')
  .test.location.parameter('D$NON_NEGATIVE_NUMERIC')
  .test.location.parameter('D$POSITIVE_NUMERIC')
  .test.location.parameter('D$NEGATIVE_NUMERIC')
})

context("ds.var()::math::location::parameter::multiple")
test_that("var (X+a) = Var(X)",
{
  connect.all.datasets()
  .test.location.parameter('D$INTEGER')
  .test.location.parameter('D$NON_NEGATIVE_INTEGER')
  .test.location.parameter('D$POSITIVE_INTEGER')
  .test.location.parameter('D$NEGATIVE_INTEGER')
  .test.location.parameter('D$NUMERIC')
  .test.location.parameter('D$NON_NEGATIVE_NUMERIC')
  .test.location.parameter('D$POSITIVE_NUMERIC')
  .test.location.parameter('D$NEGATIVE_NUMERIC')
})

context("ds.var()::math::scale::single")
test_that("var (aX) = a^2Var(X)",
{
  connect.dataset.1()
  .test.scale('D$INTEGER')
  .test.scale('D$NON_NEGATIVE_INTEGER')
  .test.scale('D$POSITIVE_INTEGER')
  .test.scale('D$NEGATIVE_INTEGER')
  .test.scale('D$NUMERIC')
  .test.scale('D$NON_NEGATIVE_NUMERIC')
  .test.scale('D$POSITIVE_NUMERIC')
  .test.scale('D$NEGATIVE_NUMERIC')
})

context("ds.var()::math::scale::multiple")
test_that("var (aX) = a^2Var(X)",
{
  connect.all.datasets()
  .test.scale('D$INTEGER')
  .test.scale('D$NON_NEGATIVE_INTEGER')
  .test.scale('D$POSITIVE_INTEGER')
  .test.scale('D$NEGATIVE_INTEGER')
  .test.scale('D$NUMERIC')
  .test.scale('D$NON_NEGATIVE_NUMERIC')
  .test.scale('D$POSITIVE_NUMERIC')
  .test.scale('D$NEGATIVE_NUMERIC')
})


