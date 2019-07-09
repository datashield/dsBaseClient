#
# Set up
#

context("ds.length::smk")

source("connection_to_datasets/init_all_datasets.R")
source("connection_to_datasets/init_smk_datasets.R")

connect.smk.dataset.sim(list('LAB_TSC'))

#
# Tests
#

context("ds.length::smk")
test_that("test_length", {
    expect_equal(ds.length(x='D$LAB_TSC')$total.number.of.observations, 9379)
    expect_equal(ds.length(x='D$LAB_TSC', type='split')$sim2, 3088)
    expect_equal(ds.length(x='D$LAB_TSC', type='split')$sim3, 4128)
})


context("ds.length::smk::errors")
test_that("length_errors", {
    expect_error(ds.length(), "Please provide the name of the input vector!", fixed=TRUE)
    expect_error(ds.length(x='D'), "The input object must be a character, factor, integer, logical or numeric vector or a list.", fixed=TRUE)
    expect_error(ds.length(x='D$LAB_TSC', type='datashield'), 'Function argument "type" has to be either "combine" or "split"', fixed=TRUE)
})

#
# Tear down
#
