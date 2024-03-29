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

context("ds.asLogical::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TSC"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

context("ds.asLogical::smk::simple test")
test_that("simple test", {
    res <- ds.asLogical("D$LAB_TSC")

    expect_equal(length(res), 2)
    expect_equal(res$is.object.created, "A data object <aslogical.newobj> has been created in all specified data sources")
    expect_equal(res$validity.check, "<aslogical.newobj> appears valid in all sources")
})

#
# Done
#

context("ds.asLogical::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "aslogical.newobj"))
})

disconnect.studies.dataset.cnsim()

context("ds.asLogical::smk::done")
