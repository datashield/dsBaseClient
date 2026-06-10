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

connect.studies.dataset.cnsim(list("LAB_TSC"))

#
# Tests
#

# context("ds.rPois::arg::test errors")
test_that("rPois_erros", {
    error.message <- ds.rPois(seed.as.integer='Value')

    expect_equal(class(error.message), 'character')
    expect_equal(error.message, 'ERROR failed: seed.as.integer must be set as an integer [numeric] or left NULL')
})

#
# Done
#

disconnect.studies.dataset.cnsim()
