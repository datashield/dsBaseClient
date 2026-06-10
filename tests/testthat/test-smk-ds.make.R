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

# context("ds.make::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TSC", "LAB_TRIG", "LAB_HDL"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.make::smk")
test_that("simple make", {
    res <- ds.make("(D$LAB_TSC*D$LAB_TRIG*D$LAB_HDL)", "maded.rand")

    expect_length(res, 2)
    expect_equal(res$is.object.created, "A data object <maded.rand> has been created in all specified data sources")
    expect_equal(res$validity.check, "<maded.rand> appears valid in all sources")
})

# context("ds.make::smk")
test_that("simple make", {
    res <- ds.make("(D$LAB_TSC*10)", "maded.rand")

    expect_length(res, 2)
    expect_equal(res$is.object.created, "A data object <maded.rand> has been created in all specified data sources")
    expect_equal(res$validity.check, "<maded.rand> appears valid in all sources")
})

# context("ds.make::smk")
test_that("simple make", {
    res <- ds.make("(D$LAB_TSC)*10", "maded.rand")

    expect_length(res, 2)
    expect_equal(res$is.object.created, "A data object <maded.rand> has been created in all specified data sources")
    expect_equal(res$validity.check, "<maded.rand> appears valid in all sources")
})

#
# Done
#

# context("ds.make::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "maded.rand"))
})

disconnect.studies.dataset.cnsim()

# context("ds.make::smk::done")
