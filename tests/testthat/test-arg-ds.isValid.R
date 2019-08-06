#
# Set up
#

connect.studies.dataset.cnsim(list("LAB_HDL"))

#
# Tests
#

context("ds.isValid()::smk::errors")
test_that("isValid errors", {
    expect_error(ds.isValid(), "Please provide the name of the input vector!", fixed=TRUE)
    expect_error(ds.isValid("D$NOT_THERE"), "The input object must be a character, factor, integer, logical or numeric vector or a dataframe or a matrix", fixed=TRUE)
})

#
# Tear down
#
