#
# Set up
#

context("dsClient::ds.dataframe")

options(datashield.variables=list('LAB_TSC','LAB_HDL'))
source("setup.R")

#
# Tests
#

context("dsClient::ds.dataframe() create a dataframe")
myvectors <- c('D$LAB_TSC', 'D$LAB_HDL')
ds.dataframe(x=myvectors)
res <- ds.ls()
test_that("dataframe_exists", {
    expect_equal(ds.ls()$sim1[2], "dframe")
    expect_equal(ds.ls()$sim2[2], "dframe")
    expect_equal(ds.ls()$sim3[2], "dframe")
})


context("dsClient::ds.dataframe() errors")
test_that("dataframe_errors", {
    expect_error(ds.dataframe(), "Please provide the name of the list that holds the input vectors!", fixed=TRUE)
})

#
# Tear down
#

source("teardown.R")