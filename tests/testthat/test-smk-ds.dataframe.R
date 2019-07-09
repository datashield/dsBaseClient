#
# Set up
#

context("ds.dataframe::smk")

source("connection_to_datasets/init_all_datasets.R")
source("connection_to_datasets/init_smk_datasets.R")

connect.smk.dataset.sim(list('LAB_TSC','LAB_HDL'))

#
# Tests
#

context("ds.dataframe::smk::create a dataframe")
myvectors <- c('D$LAB_TSC', 'D$LAB_HDL')
ds.dataframe(x=myvectors)
res <- ds.ls()
test_that("dataframe_exists", {
    expect_equal(ds.ls()$sim1[2], "dframe")
    expect_equal(ds.ls()$sim2[2], "dframe")
    expect_equal(ds.ls()$sim3[2], "dframe")
})


context("ds.dataframe::smk::errors")
test_that("dataframe_errors", {
    expect_error(ds.dataframe(), "Please provide the name of the list that holds the input vectors!", fixed=TRUE)
})

#
# Tear down
#

