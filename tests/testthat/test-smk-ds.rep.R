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

# context("ds.rep::smk::setup")

connect.studies.dataset.survival(list("survtime", "time.id", "female"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.rep::smk")
test_that("simple test", {
    res <- ds.rep("D$survtime")

    expect_length(res, 2)
    expect_equal(res$is.object.created, "A data object <seq.vect> has been created in all specified data sources")
    expect_equal(res$validity.check, "<seq.vect> appears valid in all sources")
})

#
# Done
#

# context("ds.rep::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "seq.vect"))
})

disconnect.studies.dataset.survival()

# context("ds.rep::smk::done")
