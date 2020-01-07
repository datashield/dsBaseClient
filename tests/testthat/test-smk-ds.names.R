#-------------------------------------------------------------------------------
# Copyright (c) 2018-2020 University of Newcastle upon Tyne. All rights reserved.
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

context("ds.names::smk::setup")

connect.studies.dataset.cnsim(list("DIS_DIAB","PM_BMI_CONTINUOUS","LAB_HDL", "GENDER"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

context("ds.names::smk::test errors")
ds.asCharacter(x='D$GENDER', newobj="not_a_list")
test_that("names_erros", {
    expect_error(ds.names(), "Please provide the name of the input list!", fixed=TRUE)
    expect_error(ds.names('not_a_list'), "The input object must be a list.", fixed=TRUE)
})

# context("ds.names::smk")
# ds.subsetByClass(datasources=opals, subsets='subclasses', x='D')
# names <- ds.names('subclasses')
# expected_names <- c("DIS_DIAB.level_0", "DIS_DIAB.level_1", "GENDER.level_0",   "GENDER.level_1")
# test_that("level_names", {
#   expect_equal(length(names), 3)
#   expect_equal(length(names$sim1), 4)
#   expect_equal(names$sim1, expected_names)
#   expect_equal(length(names$sim2), 4)
#   expect_equal(names$sim2, expected_names)
#   expect_equal(length(names$sim3), 4)
#   expect_equal(names$sim3, expected_names)
# })

#
# Tear down
#

context("ds.names::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "my_newobj"))
})

disconnect.studies.dataset.cnsim()

context("ds.names::smk::done")
