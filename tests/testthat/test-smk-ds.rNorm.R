#-------------------------------------------------------------------------------
# Copyright (c) 2018 University of Newcastle upon Tyne. All rights reserved.
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

connect.studies.dataset.cnsim(list("LAB_TSC"))

#
# Tests
#

context("ds.rNorm::smk::simple test")
test_that("simple test", {
    res <- ds.rNorm(samp.size = 50, mean = 10, sd = 5, newobj = "norm_dist", seed.as.integer = 27, force.output.to.k.decimal.places = 4)

    expect_length(res, 4)
    expect_length(res$integer.seed.as.set.by.source, 3)
    expect_equal(res$integer.seed.as.set.by.source[1], 27)
    expect_equal(res$integer.seed.as.set.by.source[2], 54)
    expect_equal(res$integer.seed.as.set.by.source[3], 81)
    expect_length(res$random.vector.length.by.source, 3)
    expect_equal(res$random.vector.length.by.source[1], 50)
    expect_equal(res$random.vector.length.by.source[2], 50)
    expect_equal(res$random.vector.length.by.source[3], 50)
    expect_equal(res$is.object.created, "A data object <norm_dist> has been created in all specified data sources")
    expect_equal(res$validity.check, "<norm_dist> appears valid in all sources")
})

#
# Done
#

disconnect.studies.dataset.cnsim()
