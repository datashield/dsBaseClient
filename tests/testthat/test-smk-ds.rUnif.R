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

    expect_length(res, 2)
    expect_length(res$integer.seed.as.set.by.source, 3)
    expect_equal(res$integer.seed.as.set.by.source[1], 27)
    expect_equal(res$integer.seed.as.set.by.source[2], 54)
    expect_equal(res$integer.seed.as.set.by.source[3], 81)
    expect_length(res$random.vector.length.by.source, 3)
    expect_equal(res$random.vector.length.by.source[1], 50)
    expect_equal(res$random.vector.length.by.source[2], 50)
    expect_equal(res$random.vector.length.by.source[3], 50)
    ds_expect_variables(c("D", "unif_dist"))
})

# context("ds.rUnif::smk::nonexistent object")
test_that("nonexistent server-side object", {
    expect_error(ds.rUnif(samp.size = 50, min = "nonexistent_obj", max = 1, newobj = "no.obj", seed.as.integer = 27), "There are some DataSHIELD errors, list them with datashield.errors()", fixed = TRUE)

    res.errors <- DSI::datashield.errors()

    expect_length(res.errors, 1)
    expect_match(res.errors$sim1, "The server-side object 'nonexistent_obj' does not exist", fixed = TRUE)
})

# context("ds.rUnif::smk::one value per study")
test_that("one value per study", {
    ds.rUnif(samp.size = 50, min = c(0, 10, 20), max = c(1, 11, 21), newobj = "unif_by_study", seed.as.integer = 27)

    res.mean <- ds.mean(x = "unif_by_study", type = "split")

    expect_lt(abs(as.numeric(res.mean$Mean.by.Study[1]) - 0.5), 0.5)
    expect_lt(abs(as.numeric(res.mean$Mean.by.Study[2]) - 10.5), 0.5)
    expect_lt(abs(as.numeric(res.mean$Mean.by.Study[3]) - 20.5), 0.5)
})

# context("ds.rUnif::smk::wrong number of values")
test_that("wrong number of values per study", {
    expect_error(ds.rUnif(samp.size = 50, min = c(0, 10), max = 30, newobj = "no.obj", seed.as.integer = 27), "must be length 1 or one value per study")
})

#
# Done
#

# context("ds.rUnif::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "unif_dist", "unif_by_study"))
})

disconnect.studies.dataset.cnsim()

# context("ds.rUnif::smk::done")
