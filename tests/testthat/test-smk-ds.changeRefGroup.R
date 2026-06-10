#-------------------------------------------------------------------------------
# Copyright (c) 2019-2022 University of Newcastle upon Tyne. All rights reserved.
# Copyright (c) 2022-2025 Arjuna Technologies, Newcastle upon Tyne. All rights reserved.
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

# context("ds.changeRefGroup::smk::setup")

connect.studies.dataset.cnsim(list('PM_BMI_CATEGORICAL'))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.changeRefGroup::smk")
test_that("simple changeRefGroup", {
    newNames <- c('normal', 'overweight', 'obesity')
    expect_warning(ds.recodeLevels(x='D$PM_BMI_CATEGORICAL', newCategories=newNames, newobj='bmi_new'), "'ds.recodeLevels' is deprecated.\nUse 'ds.recodeValues' instead.", fixed = TRUE)

    res <- ds.changeRefGroup(x='bmi_new', ref='obesity', newobj='bmi_ob')

    res.class  <- ds.class("bmi_ob")
    res.levels <- ds.levels(x='bmi_ob')

    expect_null(res)
    expect_length(res.class, 3)
    expect_length(res.class$sim1, 1)
    expect_equal(res.class$sim1, 'factor')
    expect_length(res.class$sim1, 1)
    expect_equal(res.class$sim2, 'factor')
    expect_length(res.class$sim1, 1)
    expect_equal(res.class$sim3, 'factor')
    expect_length(res.levels, 3)
    expect_length(res.levels$sim1, 2)
    expect_length(res.levels$sim1$ValidityMessage, 1)
    expect_equal(res.levels$sim1$ValidityMessage, "VALID ANALYSIS")
    expect_length(res.levels$sim1$Levels, 3)
    expect_equal(res.levels$sim1$Levels[1], 'obesity')
    expect_equal(res.levels$sim1$Levels[2], 'normal')
    expect_equal(res.levels$sim1$Levels[3], 'overweight')
    expect_length(res.levels$sim2, 2)
    expect_length(res.levels$sim2$ValidityMessage, 1)
    expect_equal(res.levels$sim2$ValidityMessage, "VALID ANALYSIS")
    expect_length(res.levels$sim2$Levels, 3)
    expect_equal(res.levels$sim2$Levels[1], 'obesity')
    expect_equal(res.levels$sim2$Levels[2], 'normal')
    expect_equal(res.levels$sim2$Levels[3], 'overweight')
    expect_length(res.levels$sim3, 2)
    expect_length(res.levels$sim3$ValidityMessage, 1)
    expect_equal(res.levels$sim3$ValidityMessage, "VALID ANALYSIS")
    expect_length(res.levels$sim3$Levels, 3)
    expect_equal(res.levels$sim3$Levels[1], 'obesity')
    expect_equal(res.levels$sim3$Levels[2], 'normal')
    expect_equal(res.levels$sim3$Levels[3], 'overweight')
})

#
# Done
#

# context("ds.changeRefGroup::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "bmi_new", "bmi_ob"))
})

disconnect.studies.dataset.cnsim()

# context("ds.changeRefGroup::smk::done")
