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

# context("ds.testObjExists::arg::test errors")
test_that("testObjExists_erros", {
    res <- ds.testObjExists()

    expect_equal(res, "Error: please provide the name of an object on the data servers as a character string in inverted commas")
})

#
# Done
#

disconnect.studies.dataset.cnsim()
