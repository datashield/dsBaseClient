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

context("ds.subsetByClass::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TSC", "GENDER"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

context("ds.subsetByClass::smk")
test_that("gender", {
    res <- ds.subsetByClass(x='D', subsets='subclasses')

    expect_true(is.null(res))

    check <- ds.names('subclasses')

    expect_length(check, 3)
    expect_length(check$sim1, 2)
    expect_equal(check$sim1[1], 'GENDER.level_0')
    expect_equal(check$sim1[2], 'GENDER.level_1')
    expect_length(check$sim2, 2)
    expect_equal(check$sim2[1], 'GENDER.level_0')
    expect_equal(check$sim2[2], 'GENDER.level_1')
    expect_length(check$sim3, 2)
    expect_equal(check$sim3[1], 'GENDER.level_0')
    expect_equal(check$sim3[2], 'GENDER.level_1')
})

#
# Tear down
#

context("ds.subsetByClass::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "subclasses"))
})

disconnect.studies.dataset.cnsim()

context("ds.subsetByClass::smk::done")
