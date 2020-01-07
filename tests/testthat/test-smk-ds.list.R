#-------------------------------------------------------------------------------
# Copyright (c) 2019-2020 University of Newcastle upon Tyne. All rights reserved.
#
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#
# Set up
#

context("ds.list::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TSC", "LAB_HDL"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

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

context("ds.list::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "my_newobj"))
})

disconnect.studies.dataset.cnsim()

context("ds.list::smk::down")
