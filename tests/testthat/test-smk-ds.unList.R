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

# context("ds.unList::smk::setup")

connect.studies.dataset.cnsim(list("GENDER"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.unList::smk::simple test")
test_that("simple test", {
    ds.asList(x.name="D$GENDER", newobj="GENDER.list")

    res <- ds.unList("GENDER.list")

    expect_length(res, 2)
    expect_equal(res$is.object.created, "A data object <unlist.newobj> has been created in all specified data sources")
    expect_equal(res$validity.check, "<unlist.newobj> appears valid in all sources")
})

#
# Done
#

# context("ds.unList::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "GENDER.list", "unlist.newobj"))
})

disconnect.studies.dataset.cnsim()

# context("ds.unList::smk::done")
