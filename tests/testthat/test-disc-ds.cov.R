#-------------------------------------------------------------------------------
# Copyright (c) 2018-2022 University of Newcastle upon Tyne. All rights reserved.
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

# context("ds.cov::disc::setup")

connect.studies.dataset.cnsim(list("LAB_TSC", "LAB_TRIG", "LAB_HDL", "LAB_GLUC_ADJUSTED", "GENDER"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.cov::disc")
test_that("simple D3 setup", {
    ds.dataFrameSubset('D',V1.name = 'D$GENDER', V2.name = '0', Boolean.operator = '==', newobj='D1')
    ds.dataFrameSubset('D1',V1.name = 'D1$LAB_TSC', V2.name = '3.5', Boolean.operator = '<', newobj='D2')
    ds.dataFrame(c('D2$LAB_TSC', 'D2$LAB_TRIG', 'D2$LAB_HDL', 'D2$LAB_GLUC_ADJUSTED'), newobj='D3')

    res <- ds.dim('D3')
    expect_length(res, 4)
    expect_length(res$`dimensions of D3 in sim1`, 2)
    expect_equal(res$`dimensions of D3 in sim1`[1], 15)
    expect_equal(res$`dimensions of D3 in sim1`[2], 4)
    expect_length(res$`dimensions of D3 in sim2`, 2)
    expect_equal(res$`dimensions of D3 in sim2`[1], 11)
    expect_equal(res$`dimensions of D3 in sim2`[2], 4)
    expect_length(res$`dimensions of D3 in sim3`, 2)
    expect_equal(res$`dimensions of D3 in sim3`[1], 16)
    expect_equal(res$`dimensions of D3 in sim3`[2], 4)
    expect_length(res$`dimensions of D3 in combined studies`, 2)
    expect_equal(res$`dimensions of D3 in combined studies`[1], 42)
    expect_equal(res$`dimensions of D3 in combined studies`[2], 4)
})

test_that("simple disc test, naAction='casewise.complete', type='split'", {
    res <- ds.cov('D3', naAction='casewise.complete', type='split')

    expect_length(res, 3)
    expect_length(res[[1]], 5)
    expect_length(res[[1]]$`Error message`, 1)
    expect_true(is.na(res[[1]]$`Error message`))
    expect_length(res[[2]], 5)
    expect_length(res[[2]]$`Error message`, 1)
    expect_equal(res[[2]]$`Error message`, "ERROR: The ratio of the number of variables over the number of individual-level records exceeds the allowed threshold, there is a possible risk of disclosure", fixed=TRUE)
    expect_length(res[[3]], 5)
    expect_length(res[[3]]$`Error message`, 1)
    expect_true(is.na(res[[3]]$`Error message`))
})

test_that("simple disc test, naAction='pairwise.complete', type='split'", {
#    res <- ds.cov('D3', naAction='pairwise.complete', type='split')
#
#    expect_length(res, 3)
#    expect_length(res[[1]], 5)
#    expect_length(res[[1]]$`Error message`, 1)
#    expect_true(is.na(res[[1]]$`Error message`))
#    expect_length(res[[2]], 5)
#    expect_length(res[[2]]$`Error message`, 1)
#    expect_equal(res[[2]]$`Error message`, "ERROR: The ratio of the number of variables over the number of individual-level records exceeds the allowed threshold, there is a possible risk of disclosure", fixed=TRUE)
#    expect_length(res[[3]], 5)
#    expect_length(res[[3]]$`Error message`, 1)
#    expect_true(is.na(res[[3]]$`Error message`))
})

test_that("simple disc test, naAction='casewise.complete', type='combine'", {
    res <- ds.cov('D3', naAction='casewise.complete', type='combine')

    expect_length(res, 5)
    expect_length(res$`Error message`, 3)
    expect_length(res$`Error message`[[1]], 1)
    expect_true(is.na(res$`Error message`[[1]]))
    expect_length(res$`Error message`[[2]], 1)
    expect_equal(res$`Error message`[[2]], "ERROR: The ratio of the number of variables over the number of individual-level records exceeds the allowed threshold, there is a possible risk of disclosure", fixed=TRUE)
    expect_length(res$`Error message`[[3]], 1)
    expect_true(is.na(res$`Error message`[[3]]))
})

test_that("simple disc test, naAction='pairwise.complete', type='combine'", {
#    res <- ds.cov('D3', naAction='pairwise.complete', type='combine')
#
#    expect_length(res, 5)
#    expect_length(res$`Error message`, 3)
#    expect_length(res$`Error message`[[1]], 1)
#    expect_true(is.na(res$`Error message`[[1]]))
#    expect_length(res$`Error message`[[2]], 1)
#    expect_equal(res$`Error message`[[2]], "ERROR: The ratio of the number of variables over the number of individual-level records exceeds the allowed threshold, there is a possible risk of disclosure", fixed=TRUE)
#    expect_length(res$`Error message`[[3]], 1)
#    expect_true(is.na(res$`Error message`[[3]]))
})

#
# Done
#

# context("ds.cov::disc::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "D1", "D2", "D3"))
})

disconnect.studies.dataset.survival()

# context("ds.cov::disc::done")
