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

# context("ds.length::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TSC"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.length::smk")
test_that("basic length, both", {
    res.length <- ds.length('D$LAB_TSC', type='both')

    expect_length(res.length, 4)
    expect_equal(res.length$`length of D$LAB_TSC in sim1`, 2163)
    expect_equal(res.length$`length of D$LAB_TSC in sim2`, 3088)
    expect_equal(res.length$`length of D$LAB_TSC in sim3`, 4128)
    expect_equal(res.length$`total length of D$LAB_TSC in all studies combined`, 9379)
})

test_that("basic length, split", {
    res.length <- ds.length('D$LAB_TSC', type='split')

    expect_length(res.length, 3)
    expect_equal(res.length$`length of D$LAB_TSC in sim1`, 2163)
    expect_equal(res.length$`length of D$LAB_TSC in sim2`, 3088)
    expect_equal(res.length$`length of D$LAB_TSC in sim3`, 4128)
})

test_that("basic length, combine", {
    res.length <- ds.length('D$LAB_TSC', type='combine')

    expect_length(res.length, 1)
    expect_equal(res.length$`total length of D$LAB_TSC in all studies combined`, 9379)
})

test_that("basic length, both", {
    res.length <- ds.length('D$LAB_TSC', type='both', check=TRUE)

    expect_length(res.length, 4)
    expect_equal(res.length$`length of D$LAB_TSC in sim1`, 2163)
    expect_equal(res.length$`length of D$LAB_TSC in sim2`, 3088)
    expect_equal(res.length$`length of D$LAB_TSC in sim3`, 4128)
    expect_equal(res.length$`total length of D$LAB_TSC in all studies combined`, 9379)
})

test_that("basic length, split", {
    res.length <- ds.length('D$LAB_TSC', type='split', check=TRUE)

    expect_length(res.length, 3)
    expect_equal(res.length$`length of D$LAB_TSC in sim1`, 2163)
    expect_equal(res.length$`length of D$LAB_TSC in sim2`, 3088)
    expect_equal(res.length$`length of D$LAB_TSC in sim3`, 4128)
})

test_that("basic length, combine", {
    res.length <- ds.length('D$LAB_TSC', type='combine', check=TRUE)

    expect_length(res.length, 1)
    expect_equal(res.length$`total length of D$LAB_TSC in all studies combined`, 9379)
})

#
# Done
#

# context("ds.length::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D"))
})

disconnect.studies.dataset.cnsim()

# context("ds.length::smk::done")
