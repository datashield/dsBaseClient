source("connection_to_datasets/init_all_datasets.R")
source("definition_tests/def-ds.make.R")


context("ds.make()::math::transformation::multiple")
test_that("transformation",
{
  connect.all.datasets()
  constant.value <- sample(1:200,1)
  .test.operation.mean("D$INTEGER", constant.value, "INTEGER_CREATED", "+")
  .test.operation.mean("D$INTEGER", constant.value, "INTEGER_CREATED", "-")
  .test.operation.mean("D$INTEGER", constant.value, "INTEGER_CREATED", "/")
  .test.operation.mean("D$INTEGER", constant.value, "INTEGER_CREATED", "*")
  
  constant.value <- sample(1:200,1)
  .test.operation.mean("D$POSITIVE_INTEGER", constant.value, "INTEGER_CREATED", "+")
  .test.operation.mean("D$POSITIVE_INTEGER", constant.value, "INTEGER_CREATED", "-")
  .test.operation.mean("D$POSITIVE_INTEGER", constant.value, "INTEGER_CREATED", "/")
  .test.operation.mean("D$POSITIVE_INTEGER", constant.value, "INTEGER_CREATED", "*")
  
  constant.value <- sample(1:200,1)
  .test.operation.mean("D$NEGATIVE_INTEGER", constant.value, "INTEGER_CREATED", "+")
  .test.operation.mean("D$NEGATIVE_INTEGER", constant.value, "INTEGER_CREATED", "-")
  .test.operation.mean("D$NEGATIVE_INTEGER", constant.value, "INTEGER_CREATED", "/")
  .test.operation.mean("D$NEGATIVE_INTEGER", constant.value, "INTEGER_CREATED", "*")
  
  constant.value <- sample(1:200,1)
  .test.operation.mean("D$NON_NEGATIVE_INTEGER", constant.value, "INTEGER_CREATED", "+")
  .test.operation.mean("D$NON_NEGATIVE_INTEGER", constant.value, "INTEGER_CREATED", "-")
  .test.operation.mean("D$NON_NEGATIVE_INTEGER", constant.value, "INTEGER_CREATED", "/")
  .test.operation.mean("D$NON_NEGATIVE_INTEGER", constant.value, "INTEGER_CREATED", "*")
  
  constant.value <- sample(1:200,1)
  .test.operation.mean("D$NUMERIC", constant.value, "NUMERIC_CREATED", "+")
  .test.operation.mean("D$NUMERIC", constant.value, "NUMERIC_CREATED", "-")
  .test.operation.mean("D$NUMERIC", constant.value, "NUMERIC_CREATED", "/")
  .test.operation.mean("D$NUMERIC", constant.value, "NUMERIC_CREATED", "*")
  
  constant.value <- sample(1:200,1)
  .test.operation.mean("D$POSITIVE_NUMERIC", constant.value, "NUMERIC_CREATED", "+")
  .test.operation.mean("D$POSITIVE_NUMERIC", constant.value, "NUMERIC_CREATED", "-")
  .test.operation.mean("D$POSITIVE_NUMERIC", constant.value, "NUMERIC_CREATED", "/")
  .test.operation.mean("D$POSITIVE_NUMERIC", constant.value, "NUMERIC_CREATED", "*")
  
  constant.value <- sample(1:200,1)
  .test.operation.mean("D$NEGATIVE_NUMERIC", constant.value, "NUMERIC_CREATED", "+")
  .test.operation.mean("D$NEGATIVE_NUMERIC", constant.value, "NUMERIC_CREATED", "-")
  .test.operation.mean("D$NEGATIVE_NUMERIC", constant.value, "NUMERIC_CREATED", "/")
  .test.operation.mean("D$NEGATIVE_NUMERIC", constant.value, "NUMERIC_CREATED", "*")
  
  constant.value <- sample(1:200,1)
  .test.operation.mean("D$NON_NEGATIVE_NUMERIC", constant.value, "NUMERIC_CREATED", "+")
  .test.operation.mean("D$NON_NEGATIVE_NUMERIC", constant.value, "NUMERIC_CREATED", "-")
  .test.operation.mean("D$NON_NEGATIVE_NUMERIC", constant.value, "NUMERIC_CREATED", "/")
  .test.operation.mean("D$NON_NEGATIVE_NUMERIC", constant.value, "NUMERIC_CREATED", "*")
})

