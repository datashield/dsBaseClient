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

context("ds.rep::arg::setup")

connect.studies.dataset.survival(list("survtime", "time.id", "female"))

#
# Tests
#

context("ds.rep::arg::test errors")

test_that("ds.rep erros", {
    res <- ds.rep()

    expect_equal(res, "Error: x1 must have a value which is a character string, a numeric vector on the clientside or a scalar", fixed=TRUE)
})

#
# Done
#

context("ds.rep::arg::shutdown")

disconnect.studies.dataset.survival()

context("ds.rep::arg::done")
