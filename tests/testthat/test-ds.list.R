#
# Set up
#

options(datashield.variables=list("LAB_TSC","LAB_HDL"))
context("dsClient::ds.list")

source("setup.R")

#
# Tests
#

context("dsClient::ds.list()")
myobjects <- c('D$LAB_TSC', 'D$LAB_HDL')
ds.list(x=myobjects)
type <- ds.class("newlist")$sim2
test_that("Is List", {
  expect_equal(type, "list")
})

context("dsClient::ds.list() test errors")
test_that("list_erros", {
    expect_error(ds.list(), "x=NULL. Please provide the names of the objects to coerce into a list!", fixed=TRUE)
    expect_error(ds.class(), "Please provide the name of the input object!", fixed=TRUE)
})
#
# Tear down
#

source("teardown.R")