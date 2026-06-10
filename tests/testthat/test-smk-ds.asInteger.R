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

# context("ds.asInteger::smk::setup")

connect.studies.dataset.cnsim(list("GENDER"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.asInteger::smk::simple test")
test_that("simple test", {
    res <- ds.asInteger("D$GENDER")

    expect_equal(length(res), 2)
    expect_equal(res$is.object.created, "A data object <asinteger.newobj> has been created in all specified data sources")
    expect_equal(res$validity.check, "<asinteger.newobj> appears valid in all sources")
})

#
# Done
#

# context("ds.asInteger::smk::stutdown")

test_that("stutdown", {
    ds_expect_variables(c("D", "asinteger.newobj"))
})

disconnect.studies.dataset.cnsim()

# context("ds.asInteger::smk::done")
