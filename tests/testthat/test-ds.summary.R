context("dsClient::ds.summary")

options(datashield.variables=list('LAB_TSC', 'LAB_TRIG', 'LAB_HDL', 'LAB_GLUC_ADJUSTED', 'PM_BMI_CONTINUOUS', 'DIS_CVA', 'MEDI_LPD', 'DIS_DIAB', 'DIS_AMI', 'GENDER', 'PM_BMI_CATEGORICAL'))
source("setup.R")
#
# Tests
#


context("dsClient::ds.summary: suummary of a numerical variable")

res <- ds.summary(x='D$LAB_TSC')
test_that("summary_numerical_variable", {
  expect_equal(res$sim1$class, "numeric")
  expect_equal(res$sim2$length, 3088)
  expect_equal(res$sim3$`quantiles & mean`[[4]], 5.786)
})

context("dsClient::ds.summary: suummary of a character variable")
ds.asCharacter(x='D$GENDER', newobj="a_character")
res <- ds.summary(x='a_character')
test_that("summary_character_variable", {
  expect_equal(res$sim1$class, "character")
  expect_equal(res$sim2$length, 3088)
})

context("dsClient::ds.summary: suummary of a factor variable")
res <- ds.summary(x='D$GENDER')
test_that("summary_factor_variable", {
  expect_equal(res$sim1$class, "factor")
  expect_equal(res$sim2$length, 3088)
  expect_equal(res$sim3$`count of '0'`, 2091)
  expect_equal(res$sim1$categories[[1]], "0")
  expect_equal(res$sim1$categories[[2]], "1")
})

context("dsClient::ds.summary: suummary of a data frame")
res <- ds.summary(x='D')
test_that("summary_data_frame", {
  expect_equal(res$sim1$class, "data.frame")
  expect_equal(res$sim2$`number of rows`, 3088)
  expect_equal(res$sim2$`number of columns`, 11)
  expect_equal(res$sim3$`variables held`[[11]], "PM_BMI_CATEGORICAL")
  expect_equal(res$sim1$`variables held`[[2]], "LAB_TRIG")
})

context("dsClient::ds.summary() test errors")
ds.asCharacter(x='D$LAB_TSC', newobj="not_a_numeric")
test_that("summary_erros", {
    expect_error(ds.summary(), "Please provide the name of the input vector!", fixed=TRUE)
})
#
# Tear down
#

source("teardown.R")