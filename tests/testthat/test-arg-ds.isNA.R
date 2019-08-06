#
# Set up
#

source("connection_to_datasets/init_all_datasets.R")
source("connection_to_datasets/init_smk_datasets.R")

connect.smk.dataset.sim(list("LAB_HDL"))

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
