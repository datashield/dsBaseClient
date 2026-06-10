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

# context("ds.recodeValues::smk::setup")

connect.studies.dataset.survival(list("survtime", "time.id", "female", "age.60"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.recodeValues::smk")
test_that("simple test", {
    res <- ds.recodeValues("D$survtime", values2replace.vector=c(0,1), new.values.vector=c(-10,10), newobj="recodevalues_newobj")

    expect_length(res, 2)
    expect_equal(res$is.object.created, "A data object <recodevalues_newobj> has been created in all specified data sources")
    expect_equal(res$validity.check, "<recodevalues_newobj> appears valid in all sources")

    new.res <- ds.class("recodevalues_newobj")

    expect_length(new.res, 3)
    expect_equal(new.res$survival1, "numeric")
    expect_equal(new.res$survival2, "numeric")
    expect_equal(new.res$survival3, "numeric")
})

#
# Done
#
# context("ds.recodeValues::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "recodevalues_newobj"))
})

disconnect.studies.dataset.survival()

# context("ds.recodeValues::smk::done")
