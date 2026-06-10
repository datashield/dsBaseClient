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

# context("checkClass::smk::setup")

connect.studies.dataset.cnsim(list("GENDER", "LAB_TSC", "LAB_TRIG", "LAB_HDL", "DIS_CVA", "DIS_AMI"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("checkClass::smk::simple test")
test_that("simple test", {
    res <- checkClass(ds.test_env$connections, "D$GENDER")

    expect_length(res, 1)
    expect_equal(class(res), "character")
    expect_equal(res, "factor")
})

test_that("simple test", {
    res <- checkClass(ds.test_env$connections, "D$LAB_TSC")

    expect_length(res, 1)
    expect_equal(class(res), "character")
    expect_equal(res, "numeric")
})

test_that("simple test", {
    res <- checkClass(ds.test_env$connections, "D$LAB_TRIG")

    expect_length(res, 1)
    expect_equal(class(res), "character")
    expect_equal(res, "numeric")
})

test_that("simple test", {
    res <- checkClass(ds.test_env$connections, "D$LAB_HDL")

    expect_length(res, 1)
    expect_equal(class(res), "character")
    expect_equal(res, "numeric")
})

test_that("simple test", {
    res <- checkClass(ds.test_env$connections, "D$DIS_CVA")

    expect_length(res, 1)
    expect_equal(class(res), "character")
    expect_equal(res, "factor")
})

test_that("simple test", {
    res <- checkClass(ds.test_env$connections, "D$DIS_AMI")

    expect_length(res, 1)
    expect_equal(class(res), "character")
    expect_equal(res, "factor")
})

test_that("data.frame test", {
    myvectors <- c('D$LAB_TSC', 'D$LAB_HDL')
    ds.dataFrame(x=myvectors)

    res <- checkClass(ds.test_env$connections, "dataframe.newobj")

    expect_length(res, 1)
    expect_equal(class(res), "character")
    expect_equal(res, "data.frame")
})

test_that("missing test", {
    res <- checkClass(ds.test_env$connections, "D$TEST")

    expect_length(res, 1)
    expect_equal(class(res), "character")
    expect_equal(res, "NULL")
})

#
# Done
#

# context("checkClass::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "dataframe.newobj"))
})

disconnect.studies.dataset.cnsim()

# context("checkClass::smk::done")
