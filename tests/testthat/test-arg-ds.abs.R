#
# Set up
#

connect.studies.dataset.cnsim(list("LAB_TSC"))

#
# Tests
#

# context("ds.abs::arg::test errors")
test_that("abs_errors", {
    expect_error(ds.abs(), "must be the name of the input object", fixed=TRUE)
})

#
# Done
#

disconnect.studies.dataset.cnsim()
