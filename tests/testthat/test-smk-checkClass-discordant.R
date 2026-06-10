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

# context("checkClass::smk::discordant::setup")

connect.discordant.dataset.simple(list("A", "B", "C"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("checkClass::smk::discordant")
test_that("simple test, discordant dataset A", {
    expect_error(checkClass(ds.test_env$connections, "D$A"), " End of process!", fixed=TRUE)
})

test_that("simple test, discordant dataset B", {
    expect_error(checkClass(ds.test_env$connections, "D$B"), " End of process!", fixed=TRUE)
})

test_that("simple test, discordant dataset C", {
    expect_error(checkClass(ds.test_env$connections, "D$C"), " End of process!", fixed=TRUE)
})

#
# Done
#

# context("checkClass::smk::discordant::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D"))
})

disconnect.discordant.dataset.simple()

# context("checkClass::smk::discordant::done")
