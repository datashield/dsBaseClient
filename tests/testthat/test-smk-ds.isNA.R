#
# Set up
#

context("ds.isNA::smk")

source("connection_to_datasets/init_all_datasets.R")
source("connection_to_datasets/init_smk_datasets.R")

connect.smk.dataset.sim(list("LAB_HDL"))

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


context("ds.isNA()::smk::errors")
test_that("isNA_errors", {
    expect_error(ds.isNA(), "Please provide the name of the input vector!", fixed=TRUE)
})

#
# Tear down
#
