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

context("ds.rPois::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TSC"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

context("ds.rPois::smk::simple test")
test_that("simple test", {
    res <- ds.rPois(samp.size = 50, lambda = 1, newobj = "pois_dist", seed.as.integer = 27)

    expect_length(res, 4)
    expect_length(res$integer.seed.as.set.by.source, 3)
    expect_equal(res$integer.seed.as.set.by.source[1], 27)
    expect_equal(res$integer.seed.as.set.by.source[2], 54)
    expect_equal(res$integer.seed.as.set.by.source[3], 81)
    expect_length(res$random.vector.length.by.source, 3)
    expect_equal(res$random.vector.length.by.source[1], 50)
    expect_equal(res$random.vector.length.by.source[2], 50)
    expect_equal(res$random.vector.length.by.source[3], 50)
    expect_equal(res$is.object.created, "A data object <pois_dist> has been created in all specified data sources")
    expect_equal(res$validity.check, "<pois_dist> appears valid in all sources")
})

#
# Done
#

context("ds.rPois::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "pois_dist"))
})

disconnect.studies.dataset.cnsim()

context("ds.rPois::smk::done")
