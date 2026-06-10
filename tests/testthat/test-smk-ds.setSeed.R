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

# context("ds.setSeed::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TSC"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.setSeed::smk")
test_that("basic setSeed", {
    res.setSeed <- ds.setSeed(1234)

    expect_length(res.setSeed, 2)
    expect_equal(res.setSeed$status.message, "Trigger integer to prime random seed = 1234")
    expect_length(res.setSeed$seed.as.set, 3)
    expect_length(res.setSeed$seed.as.set$sim1, 1)
    expect_length(res.setSeed$seed.as.set$sim1$seed.as.set, 626)
    expect_length(res.setSeed$seed.as.set$sim2, 1)
    expect_length(res.setSeed$seed.as.set$sim2$seed.as.set, 626)
    expect_length(res.setSeed$seed.as.set$sim3, 1)
    expect_length(res.setSeed$seed.as.set$sim3$seed.as.set, 626)
})

#
# Done
#

# context("ds.setSeed::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D"))
})

disconnect.studies.dataset.cnsim()

# context("ds.setSeed::smk::done")
