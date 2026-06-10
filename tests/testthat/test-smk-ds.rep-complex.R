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

# context("ds.rep::smk::complex setup")

connect.studies.dataset.cnsim(list("LAB_TSC", "GENDER"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.rep::smk::complex")
test_that("complex test", {

    res1 <- ds.rep(x1 = 4, times = 6, length.out = NA, each = 1, source.x1 = "clientside", source.times = "c", source.length.out = NULL, source.each = "c", x1.includes.characters = FALSE, newobj = "rep1.seq")
   
    expect_length(res1, 2)
    expect_equal(res1$is.object.created, "A data object <rep1.seq> has been created in all specified data sources")
    expect_equal(res1$validity.check, "<rep1.seq> appears valid in all sources")

    res2 <- ds.rep(x1 = "lung", times = 6, length.out = 7, each = 1, source.x1 = "clientside", source.times = "c", source.length.out = "c", source.each = "c", x1.includes.characters = TRUE, newobj = "rep2.seq")

    expect_length(res2, 2)
    expect_equal(res2$is.object.created, "A data object <rep2.seq> has been created in all specified data sources")
    expect_equal(res2$validity.check, "<rep2.seq> appears valid in all sources")
})

#
# Done
#

# context("ds.rep::smk::complex shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "rep1.seq", "rep2.seq"))
})

disconnect.studies.dataset.cnsim()

# context("ds.rep::smk::complex done")
