source("connection_to_datasets/init_all_datasets.R")
source("definition_tests/def-ds.asFactor.R")

context("ds.asFactor()::math::multiple")
test_that("every factor should be unique",
{
  connect.all.datasets()
  .test.uniqueness('D$INTEGER','INT.f')
  .test.uniqueness('D$NON_NEGATIVE_INTEGER','NON_NEG_INT.f')
  .test.uniqueness('D$POSITIVE_INTEGER', 'POS_INT.f')
  .test.uniqueness('D$NEGATIVE_INTEGER','NEG_INT.f')
  .test.uniqueness('D$NUMERIC','NUM.f')
  .test.uniqueness('D$NON_NEGATIVE_NUMERIC','NON_NEG_NUM.f')
  .test.uniqueness('D$POSITIVE_NUMERIC','POS_NUM.f')
  .test.uniqueness('D$NEGATIVE_NUMERIC','NEG_NUM.f')
  .test.uniqueness('D$FACTOR_INTEGER', 'FACTOR_INT.f')
})

context("ds.asFactor()::math::multiple")
test_that("every factor should be unique",
{
   connect.dataset.1()
   .test.uniqueness('D$INTEGER','INT.f')
   .test.uniqueness('D$NON_NEGATIVE_INTEGER','NON_NEG_INT.f')
   .test.uniqueness('D$POSITIVE_INTEGER', 'POS_INT.f')
   .test.uniqueness('D$NEGATIVE_INTEGER','NEG_INT.f')
   .test.uniqueness('D$NUMERIC','NUM.f')
   .test.uniqueness('D$NON_NEGATIVE_NUMERIC','NON_NEG_NUM.f')
   .test.uniqueness('D$POSITIVE_NUMERIC','POS_NUM.f')
   .test.uniqueness('D$NEGATIVE_NUMERIC','NEG_NUM.f')
   .test.uniqueness('D$FACTOR_INTEGER', 'FACTOR_INT.f')
})
