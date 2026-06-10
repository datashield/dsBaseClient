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

# context("ds.class::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TSC", "DIS_CVA"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.class::smk")
test_that("simple class", {
    res1 <- ds.class("D$LAB_TSC")

    expect_length(res1, 3)
    expect_length(res1$sim1, 1)
    expect_equal(res1$sim1, "numeric")
    expect_length(res1$sim2, 1)
    expect_equal(res1$sim2, "numeric")
    expect_length(res1$sim3, 1)
    expect_equal(res1$sim3, "numeric")

    res2 <- ds.class("D$DIS_CVA")

    expect_length(res2, 3)
    expect_length(res2$sim1, 1)
    expect_equal(res2$sim1, "factor")
    expect_length(res2$sim2, 1)
    expect_equal(res2$sim2, "factor")
    expect_length(res2$sim3, 1)
    expect_equal(res2$sim3, "factor")
})

#
# Done
#

# context("ds.class::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D"))
})

disconnect.studies.dataset.cnsim()

# context("ds.class::smk::done")
