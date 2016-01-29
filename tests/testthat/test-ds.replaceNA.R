context("dsBaseClient::ds.replaceNA")

options(datashield.variables=list("LAB_HDL"))
source("setup.R")
#
# Tests
#

context("dsBaseClient::ds.replaceNA")
meanVals <- ds.mean(x='D$LAB_HDL', type='split')
ds.replaceNA(x='D$LAB_HDL', forNA=meanVals, newobj='HDL.noNA')
before <- ds.numNA(x='D$LAB_HDL')
after <- ds.numNA(x='HDL.noNA')
test_that("replaceNA HDL", {
  expect_equal(before[[1]], 360)
  expect_equal(before[[2]], 555)
  expect_equal(before[[3]], 655)
  expect_equal(after[[1]], 0)
  expect_equal(after[[2]], 0)
  expect_equal(after[[3]], 0)
})


context("dsBaseClient::ds.replaceNA() test errors")
test_that("replaceNA_erros", {
    expect_error(ds.replaceNA(), "Please provide the name of a vector!", fixed=TRUE)
    expect_error(ds.numNA(), "Please provide the name of a vector!", fixed=TRUE)

})
#
# Tear down
#

source("teardown.R")