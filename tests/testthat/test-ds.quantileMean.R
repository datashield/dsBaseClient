#
# Set up
#

context("dsClient::ds.quantileMean.R")

options(datashield.variables=list('LAB_HDL'))
source("setup.R")
#
# Tests
#

context("dsClient::ds.quantileMean() standard")

res <- ds.quantileMean(x='D$LAB_HDL')
test_that("quantileMean", {
  expect_equal(res[[2]], 1.24763444743245, tolerance = .0000000000001)
  expect_equal(res[[8]], 1.56761884325778, tolerance = .0000000000001)
})

context("dsClient::ds.quantileMean() split")
ds.assign("D$LAB_HDL", "hdl")
res <- ds.quantileMean(x='hdl', type='split')
test_that("quantileMean_split", {
  expect_equal(res$sim1[[8]], 1.56941631558514, tolerance = .0000000000001)
  expect_equal(res$sim2[[5]], 1.84, tolerance = .0000000000001)
  expect_equal(res$sim3[[7]], 2.2442, tolerance = .0000000000001)
})



context("dsClient::ds.quantileMean() test errors")
ds.asCharacter(x='D$LAB_HDL', newobj="not_a_numeric")
test_that("quantileMean_erros", {
    expect_error(ds.quantileMean(), "Please provide the name of the input vector!", fixed=TRUE)
    expect_error(ds.quantileMean(x='D$LAB_HDL', type='datashield'), 'Function argument "type" has to be either "combine" or "split"', fixed=TRUE)
    expect_error(ds.quantileMean(x='not_a_numeric'), "The input object must be an integer or numeric vector.", fixed=TRUE)
})
#
# Tear down
#

source("teardown.R")