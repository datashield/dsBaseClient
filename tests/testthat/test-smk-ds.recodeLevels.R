#-------------------------------------------------------------------------------
# Copyright (c) 2018 University of Newcastle upon Tyne. All rights reserved.
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

context("ds.recodeLevels::smk::setup")

connect.studies.dataset.cnsim(list("PM_BMI_CATEGORICAL"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

context("ds.recodeLevels::smk")
ds.recodeLevels(x='D$PM_BMI_CATEGORICAL', newCategories=c('normal', 'overweight', 'obesity'), newobj='bmi_new')
levels <- ds.levels(x='bmi_new')
test_that("new levels", {
    expected <- c("normal", "overweight", "obesity")
    expect_length(levels, 3)
    expect_length(levels$sim1, 3)
    expect_equal(levels$sim1, expected)
    expect_length(levels$sim2, 3)
    expect_equal(levels$sim2, expected)
    expect_length(levels$sim3, 3)
    expect_equal(levels$sim3, expected)
})

context("ds.recodeLevels::smk::no connections or newobj")
ds.recodeLevels(x='D$PM_BMI_CATEGORICAL', newCategories=c('normal', 'overweight', 'obesity'))
levels <- ds.levels(x='PM_BMI_CATEGORICAL_new')
test_that("new levels auto", {
    expected <- c("normal", "overweight", "obesity")
    expect_length(levels, 3)
    expect_length(levels$sim1, 3)
    expect_equal(levels$sim1, expected)
    expect_length(levels$sim2, 3)
    expect_equal(levels$sim2, expected)
    expect_length(levels$sim3, 3)
    expect_equal(levels$sim3, expected)
})

#
# Tear down
#

context("ds.recodeLevels::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "bmi_new", "PM_BMI_CATEGORICAL_new"))
})

disconnect.studies.dataset.cnsim()

context("ds.recodeLevels::smk::done")
