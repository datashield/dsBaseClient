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

# context("ds.rPois::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TSC"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.rPois::smk::simple test")
test_that("simple test", {
    res <- ds.rPois(samp.size = 50, lambda = 1, newobj = "pois_dist", seed.as.integer = 27)

    expect_length(res, 2)
    expect_length(res$integer.seed.as.set.by.source, 3)
    expect_equal(res$integer.seed.as.set.by.source[1], 27)
    expect_equal(res$integer.seed.as.set.by.source[2], 54)
    expect_equal(res$integer.seed.as.set.by.source[3], 81)
    expect_length(res$random.vector.length.by.source, 3)
    expect_equal(res$random.vector.length.by.source[1], 50)
    expect_equal(res$random.vector.length.by.source[2], 50)
    expect_equal(res$random.vector.length.by.source[3], 50)
    ds_expect_variables(c("D", "pois_dist"))
})

# context("ds.rPois::smk::nonexistent object")
test_that("nonexistent server-side object", {
    expect_error(ds.rPois(samp.size = 50, lambda = "nonexistent_obj", newobj = "no.obj", seed.as.integer = 27), "There are some DataSHIELD errors, list them with datashield.errors()", fixed = TRUE)

    res.errors <- DSI::datashield.errors()

    expect_length(res.errors, 1)
    expect_match(res.errors$sim1, "The server-side object 'nonexistent_obj' does not exist", fixed = TRUE)
})

# context("ds.rPois::smk::one value per study")
test_that("one value per study", {
    ds.rPois(samp.size = 50, lambda = c(1, 50, 100), newobj = "pois_by_study", seed.as.integer = 27)

    res.mean <- ds.mean(x = "pois_by_study", type = "split")

    expect_lt(abs(as.numeric(res.mean$Mean.by.Study[1]) - 1), 0.5)
    expect_lt(abs(as.numeric(res.mean$Mean.by.Study[2]) - 50), 3)
    expect_lt(abs(as.numeric(res.mean$Mean.by.Study[3]) - 100), 5)
})

# context("ds.rPois::smk::wrong number of values")
test_that("wrong number of values per study", {
    expect_error(ds.rPois(samp.size = 50, lambda = c(1, 50), newobj = "no.obj", seed.as.integer = 27), "must be length 1 or one value per study")
})

#
# Done
#

# context("ds.rPois::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "pois_dist", "pois_by_study"))
})

disconnect.studies.dataset.cnsim()

# context("ds.rPois::smk::done")
