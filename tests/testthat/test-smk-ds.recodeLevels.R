#-------------------------------------------------------------------------------
# Copyright (c) 2018-2021 University of Newcastle upon Tyne. All rights reserved.
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
test_that("new levels", {
    ds.recodeLevels(x='D$PM_BMI_CATEGORICAL', newCategories=c('normal', 'overweight', 'obesity'), newobj='bmi_new')
    levels <- ds.levels(x='bmi_new')

    expected <- c("normal", "overweight", "obesity")
    expect_length(levels, 3)
    expect_length(levels$sim1$Levels, 3)
    expect_equal(levels$sim1$Levels, expected)
    expect_length(levels$sim2$Levels, 3)
    expect_equal(levels$sim2$Levels, expected)
    expect_length(levels$sim3$Levels, 3)
    expect_equal(levels$sim3$Levels, expected)
})

context("ds.recodeLevels::smk::no connections or newobj")
test_that("new levels auto", {
    ds.recodeLevels(x='D$PM_BMI_CATEGORICAL', newCategories=c('normal', 'overweight', 'obesity'))
    levels <- ds.levels(x='PM_BMI_CATEGORICAL_new')

    expected <- c("normal", "overweight", "obesity")
    expect_length(levels, 3)
    expect_length(levels$sim1$Levels, 3)
    expect_equal(levels$sim1$Levels, expected)
    expect_length(levels$sim2$Levels, 3)
    expect_equal(levels$sim2$Levels, expected)
    expect_length(levels$sim3$Levels, 3)
    expect_equal(levels$sim3$Levels, expected)
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
