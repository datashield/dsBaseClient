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

connect.studies.dataset.cnsim(list("DIS_CVA", "DIS_AMI", "LAB_TSC", "LAB_HDL"))

#
# Tests
#

# context("ds.table::arg::test rvar has value")
test_that("table rvar has value", {
    expect_error(ds.table(), "must be a character string that names the row variable")
    
})

# context("ds.table::arg::test cvar has value")
test_that("table cvar has value", {
  expect_error(ds.table(rvar="D$LAB_TSC", cvar=30), "must be a character string that names the column variable")
  
})

# context("ds.table::arg::test stvar has value")
test_that("table stvar has value", {
  expect_error(ds.table(rvar="D$LAB_TSC", cvar=NULL, stvar=25), "must be a character string that names the variable")
  
})

# context("ds.table::arg::test stvar has CORRECT value")
test_that("table useNA has value", {
  expect_error(ds.table(rvar="D$LAB_TSC", cvar=NULL, stvar=NULL, useNA = c("BEN")), "must be \"no\" or \"always\"", fixed = TRUE)
})

# context("ds.table::arg::test force.nfilter has value")
test_that("table force.nfilter has value", {
  expect_error(ds.table(rvar="D$LAB_TSC", cvar=NULL, stvar=NULL, useNA = c("ifany"), force.nfilter =30), "must be \"no\" or \"always\"", fixed = TRUE)
})


#
# Done
#

disconnect.studies.dataset.cnsim()
