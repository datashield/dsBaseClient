#
# Set up
#

context("dsClient::ds.isNA")

options(datashield.variables=list("LAB_HDL"))
source("setup.R")

#
# Tests
#

context("dsClient::ds.isNA")
res <- ds.isNA(x='D$LAB_HDL')
test_that("isNA", {
    expect_false(res$sim1)
    expect_false(res$sim1)
    expect_false(res$sim1)
})


context("dsClient::ds.isNA() errors")
test_that("isNA_errors", {
    expect_error(ds.isNA(), "Please provide the name of the input vector!", fixed=TRUE)
})

#
# Tear down
#

source("teardown.R")