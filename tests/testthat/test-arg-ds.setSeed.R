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

connect.studies.dataset.cnsim(list("LAB_TSC"))

#
# Tests
#

context("ds.setSeed::arg::test errors")
test_that("setSeed_erros", {
    res <- ds.setSeed(seed.as.integer="Test")

    expect_equal(res, "ERROR terminated: seed.as.integer must be set as an integer [numeric] or as being NULL")
})

#
# Done
#

disconnect.studies.dataset.cnsim()
