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

# context("ds.ls::arg::setup")

connect.studies.dataset.cnsim(list("LAB_TSC", "LAB_TRIG"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.ls::arg")
test_that("containing escape sequence", {
    res1 <- ds.ls(search.filter="_:A:_Environment")

    expect_length(res1, 1)
    expect_equal(res1, "Warning: Code replacing wildcard (i.e. '*') is '_:A:_' but this appears in your original search filter string - please respecify")

    res2 <- ds.ls(search.filter="Envir_:A:_oment")

    expect_length(res2, 1)
    expect_equal(res2, "Warning: Code replacing wildcard (i.e. '*') is '_:A:_' but this appears in your original search filter string - please respecify")

    res3 <- ds.ls(search.filter="Environment_:A:_")

    expect_length(res3, 1)
    expect_equal(res3, "Warning: Code replacing wildcard (i.e. '*') is '_:A:_' but this appears in your original search filter string - please respecify")
})

#
# Shutdown
#

# context("ds.ls::arg::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D"))
})

disconnect.studies.dataset.cnsim()

#
# Done
#

# context("ds.ls::arg::done")
