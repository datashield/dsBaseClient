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

# context("ds.rUnif::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TSC"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.rUnif::smk::simple test")
test_that("simple test", {
    res <- ds.rUnif(samp.size = 50, min = 0, max = 1, newobj = "unif_dist", seed.as.integer = 27, force.output.to.k.decimal.places = 4)

    expect_length(res, 4)
    expect_length(res$integer.seed.as.set.by.source, 3)
    expect_equal(res$integer.seed.as.set.by.source[1], 27)
    expect_equal(res$integer.seed.as.set.by.source[2], 54)
    expect_equal(res$integer.seed.as.set.by.source[3], 81)
    expect_length(res$random.vector.length.by.source, 3)
    expect_equal(res$random.vector.length.by.source[1], 50)
    expect_equal(res$random.vector.length.by.source[2], 50)
    expect_equal(res$random.vector.length.by.source[3], 50)
    expect_equal(res$is.object.created, "A data object <unif_dist> has been created in all specified data sources")
    expect_equal(res$validity.check, "<unif_dist> appears valid in all sources")
})

#
# Done
#

# context("ds.rUnif::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "unif_dist"))
})

disconnect.studies.dataset.cnsim()

# context("ds.rUnif::smk::done")
