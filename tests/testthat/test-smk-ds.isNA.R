#
# Set up
#

connect.studies.dataset.cnsim(list("LAB_HDL"))

#
# Tests
#

context("ds.isNA::smk")
res <- ds.isNA(x='D$LAB_HDL')
test_that("isNA", {
    expect_false(res$sim1)
    expect_false(res$sim1)
    expect_false(res$sim1)
})

#
# Tear down
#
