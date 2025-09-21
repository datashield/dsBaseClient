#-------------------------------------------------------------------------------
# Copyright (c) 2018-2022 University of Newcastle upon Tyne. All rights reserved.
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

context("ds.subset::smk::setup")

connect.studies.dataset.cnsim(list("DIS_DIAB","PM_BMI_CONTINUOUS","LAB_HDL", "GENDER"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

context("ds.subset::smk::generate a subset of the assigned table (by default the table is named 'D') with the first 50 observations and the two first columns")
ds.subset(datasources=ds.test_env$connections, subset='subD', x='D', rows=c(1:50), cols=c(1,2))
res <- ds.exists('subD')
test_that("subD_exists", {
    expect_length(res, 3)
    expect_true(res$sim1)
    expect_true(res$sim2)
    expect_true(res$sim3)
})

context("ds.subset::smk::generate a subset of the assigned table (by default the table is named 'D') with the first 50 observations and the two first columns referred to by their names")
ds.subset(subset='subD2', x='D', rows=c(1:50), cols = c('DIS_DIAB','PM_BMI_CONTINUOUS'))
res <- ds.exists('subD2')
test_that("subD2_exists", {
    expect_length(res, 3)
    expect_true(res$sim1)
    expect_true(res$sim2)
    expect_true(res$sim3)
})

context("ds.subset::smk::generate a subset of the table D with bmi values greater than or equal to 25.")
ds.subset(datasources=ds.test_env$connections, subset='subD3', x='D', logical='PM_BMI_CONTINUOUS>=', threshold=25)
res <- ds.exists('subD3')
test_that("subD3_exists", {
    expect_length(res, 3)
    expect_true(res$sim1)
    expect_true(res$sim2)
    expect_true(res$sim3)
})

# context("ds.subset::smk::get the logarithmic values of the variable 'lab_hdl' and generate a subset with the first 50 observations of that new vector.")
# ds.assign(toAssign='log(D$LAB_HDL)', newobj='logHDL')
# ds.subset(datasources=ds.test_env$connections, subset="subLAB_HDL", x="logHDL", rows=c(1:50))
# res <- ds.exists('subLAB_HDL')
# test_that("subLAB_HDL_exists", {
#     expect_length(res, 3)
#     expect_true(res$sim1)
#     expect_true(res$sim2)
#     expect_true(res$sim3)
# })

# context("ds.subset::smk::get the variable 'PM_BMI_CONTINUOUS' from the dataframe 'D' and generate a subset bmi vector with bmi values greater than or equal to 25")
# ds.assign(toAssign='D$PM_BMI_CONTINUOUS', newobj='BMI')
# ds.subset(datasources=ds.test_env$connections, subset='subBMI', x='BMI', logical='>=', threshold=25)
# res <- ds.exists('subBMI')
# test_that("subBMI_exists", {
#     expect_length(res, 3)
#     expect_true(res$sim1)
#     expect_true(res$sim2)
#     expect_true(res$sim3)
# })

#
# Tear down
#

context("ds.subset::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "subD", "subD2", "subD3"))
})

disconnect.studies.dataset.cnsim()

context("ds.subset::smk::done")
