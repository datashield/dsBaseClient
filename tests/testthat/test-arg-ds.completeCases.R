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

context("ds.completeCases::arg::test errors")
test_that("completeCases_errors", {
  res <- ds.completeCases()
  expect_equal(res, "Error: x1 must be a character string naming a serverside data.frame, matrix or vector", fixed = TRUE)
})

#
# Done
#

disconnect.studies.dataset.cnsim()
