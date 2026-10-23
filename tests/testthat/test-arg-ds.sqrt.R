#
# Set up
#

connect.studies.dataset.cnsim(list("LAB_TSC"))

#
# Tests
#

# context("ds.sqrt::arg::test errors")
test_that("sqrt_errors", {
    expect_error(ds.sqrt(), "Please provide the name of the input object!", fixed=TRUE)
})

#
# Done
#

disconnect.studies.dataset.cnsim()
