#-------------------------------------------------------------------------------
# Copyright (c) 2019-2022 University of Newcastle upon Tyne. All rights reserved.
# Copyright (c) 2022 Arjuna Technologies, Newcastle upon Tyne. All rights reserved.
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

context("ds.dataFrameFill::smk::donothing setup")

connect.studies.dataset.cnsim(list("LAB_TSC", "LAB_TRIG", "LAB_HDL"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

context("ds.dataFrameFill::smk::donothing dataframes")
test_that("dataFrameFill_donothing", {
    colnamesD <- ds.colnames('D')

    expect_length(colnamesD, 3)
    expect_length(colnamesD$sim1, 3)
    expect_equal(colnamesD$sim1[1], "LAB_TSC")
    expect_equal(colnamesD$sim1[2], "LAB_TRIG")
    expect_equal(colnamesD$sim1[3], "LAB_HDL")
    expect_length(colnamesD$sim2, 3)
    expect_equal(colnamesD$sim2[1], "LAB_TSC")
    expect_equal(colnamesD$sim2[2], "LAB_TRIG")
    expect_equal(colnamesD$sim2[3], "LAB_HDL")
    expect_length(colnamesD$sim3, 3)
    expect_equal(colnamesD$sim3[1], "LAB_TSC")
    expect_equal(colnamesD$sim3[2], "LAB_TRIG")
    expect_equal(colnamesD$sim3[3], "LAB_HDL")

    expect_error(ds.dataFrameFill(df.name="D", newobj="filled_df"), "The dataframes have the same variables. There are no missing variables to fill!")
})

#
# Done
#

context("ds.dataFrameFill::smk::donothing shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D"))
})

disconnect.studies.dataset.cnsim()

context("ds.dataFrameFill::smk::donothing done")
