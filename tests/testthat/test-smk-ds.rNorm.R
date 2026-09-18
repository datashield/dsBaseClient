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

# context("ds.rNorm::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TSC"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.rNorm::smk::simple test")
test_that("simple test", {
    res <- ds.rNorm(samp.size = 50, mean = 10, sd = 5, newobj = "norm_dist", seed.as.integer = 27, force.output.to.k.decimal.places = 4)

    expect_length(res, 2)
    expect_length(res$integer.seed.as.set.by.source, 3)
    expect_equal(res$integer.seed.as.set.by.source[1], 27)
    expect_equal(res$integer.seed.as.set.by.source[2], 54)
    expect_equal(res$integer.seed.as.set.by.source[3], 81)
    expect_length(res$random.vector.length.by.source, 3)
    expect_equal(res$random.vector.length.by.source[1], 50)
    expect_equal(res$random.vector.length.by.source[2], 50)
    expect_equal(res$random.vector.length.by.source[3], 50)
    ds_expect_variables(c("D", "norm_dist"))
})

# context("ds.rNorm::smk::nonexistent object")
test_that("nonexistent server-side object", {
    expect_error(ds.rNorm(samp.size = 50, mean = "nonexistent_obj", sd = 5, newobj = "no.obj", seed.as.integer = 27), "There are some DataSHIELD errors, list them with datashield.errors()", fixed = TRUE)

    res.errors <- DSI::datashield.errors()

    expect_length(res.errors, 1)
    expect_match(res.errors$sim1, "The server-side object 'nonexistent_obj' does not exist", fixed = TRUE)
})

# context("ds.rNorm::smk::one value per study")
test_that("one value per study", {
    ds.rNorm(samp.size = 50, mean = c(0, 100, 200), sd = 1, newobj = "norm_by_study", seed.as.integer = 27)

    res.mean <- ds.mean(x = "norm_by_study", type = "split")

    expect_lt(abs(as.numeric(res.mean$Mean.by.Study[1]) - 0), 1)
    expect_lt(abs(as.numeric(res.mean$Mean.by.Study[2]) - 100), 1)
    expect_lt(abs(as.numeric(res.mean$Mean.by.Study[3]) - 200), 1)
})

# context("ds.rNorm::smk::wrong number of values")
test_that("wrong number of values per study", {
    expect_error(ds.rNorm(samp.size = 50, mean = c(0, 100), sd = 1, newobj = "no.obj", seed.as.integer = 27), "must be length 1 or one value per study")
})

#
# Done
#

# context("ds.rNorm::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "norm_dist", "norm_by_study"))
})

disconnect.studies.dataset.cnsim()

# context("ds.rNorm::smk::done")
