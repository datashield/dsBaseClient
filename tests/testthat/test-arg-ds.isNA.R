#
# Set up
#

connect.studies.dataset.cnsim(list("LAB_HDL"))

#
# Tests
#

context("ds.isNA()::smk::errors")
test_that("isNA errors", {
    expect_error(ds.isNA(), "Please provide the name of the input vector!", fixed=TRUE)
})

#
# Tear down
#
