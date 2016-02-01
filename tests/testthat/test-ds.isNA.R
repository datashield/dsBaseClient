#
# Set up
#

context("dsBaseClient::ds.isNA")

options(datashield.variables=list("LAB_HDL"))
source("setup.R")

#
# Tests
#

context("dsBaseClient::ds.isNA")
res <- ds.isNA(x='D$LAB_HDL')
test_that("isNA", {
    expect_false(res$sim1)
    expect_false(res$sim1)
    expect_false(res$sim1)
})


context("dsBaseClient::ds.isNA() errors")
test_that("isNA_errors", {
    expect_error(ds.isNA(), "Please provide the name of the input vector!", fixed=TRUE)
})

#
# Tear down
#

source("teardown.R")