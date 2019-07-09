#
# Set up
#

context("ds.list::smk")

source("connection_to_datasets/init_all_datasets.R")
source("connection_to_datasets/init_smk_datasets.R")

connect.smk.dataset.sim(list("D$LAB_TSC', 'D$LAB_HDL"))

#
# Tests
#

context("ds.list::smk")
myobjects <- c('D$LAB_TSC', 'D$LAB_HDL')
ds.list(x=myobjects)
type <- ds.class("newlist")$sim2
test_that("Is List", {
  expect_equal(type, "list")
})

context("ds.list::smk::test errors")
test_that("list_erros", {
    expect_error(ds.list(), "x=NULL. Please provide the names of the objects to coerce into a list!", fixed=TRUE)
    expect_error(ds.class(), "Please provide the name of the input object!", fixed=TRUE)
})
#
# Tear down
#

