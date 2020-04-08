#-------------------------------------------------------------------------------
# Copyright (c) 2019-2020 University of Newcastle upon Tyne. All rights reserved.
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

context("ds.ls::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TSC", "LAB_TRIG"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

context("ds.ls::smk")
test_that("simple ls", {
    res <- ds.ls()

    expect_length(res, 3)
    expect_length(res$sim1, 2)
    expect_equal(res$sim1$environment.searched, "R_GlobalEnv")
    expect_length(res$sim1$objects.found, 1)
    expect_equal(res$sim1$objects.found[1], "D")
    expect_length(res$sim2, 2)
    expect_equal(res$sim2$environment.searched, "R_GlobalEnv")
    expect_length(res$sim2$objects.found, 1)
    expect_equal(res$sim2$objects.found[1], "D")
    expect_length(res$sim3, 2)
    expect_equal(res$sim3$environment.searched, "R_GlobalEnv")
    expect_length(res$sim3$objects.found, 1)
    expect_equal(res$sim3$objects.found[1], "D")
})

#
# Shutdown
#

context("ds.ls::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D"))
})

disconnect.studies.dataset.cnsim()

#
# Done
#

context("ds.ls::smk::done")
