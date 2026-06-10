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

# context("ds.unique::smk::setup")

connect.studies.dataset.cnsim(list("GENDER"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.unique::smk::simple test")
test_that("simple test", {
    original.newobj.length <- ds.length("D$GENDER")

    expect_length(original.newobj.length, 4)
    expect_equal(original.newobj.length$`length of D$GENDER in sim1`, 2163)
    expect_equal(original.newobj.length$`length of D$GENDER in sim2`, 3088)
    expect_equal(original.newobj.length$`length of D$GENDER in sim3`, 4128)
    expect_equal(original.newobj.length$`total length of D$GENDER in all studies combined`, 9379)

    res <- ds.unique("D$GENDER")

    expect_length(res, 0)

    unique.newobj.length <- ds.length("unique.newobj")

    expect_length(unique.newobj.length, 4)
    expect_equal(unique.newobj.length$`length of unique.newobj in sim1`, 2)
    expect_equal(unique.newobj.length$`length of unique.newobj in sim2`, 2)
    expect_equal(unique.newobj.length$`length of unique.newobj in sim3`, 2)
    expect_equal(unique.newobj.length$`total length of unique.newobj in all studies combined`, 6)
})

#
# Done
#

# context("ds.unique::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "unique.newobj"))
})

disconnect.studies.dataset.cnsim()

#context("ds.unique::smk::done")
