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
    res <- ds.table()
    
    expect_equal(res, "Error: rvar must have a value which is a character string naming the row variable for the table", fixed=TRUE)
})

# context("ds.table::arg::test rvar is not a variable")
test_that("table rvar is not a variable", {
    expect_error(ds.table(rvar="D$NOT_VARIABLE"), "The input object D$NOT_VARIABLE is not defined in sim1, sim2, sim3!", fixed = TRUE)

    res.errors <- DSI::datashield.errors()

    expect_length(res.errors, 0)
})

# context("ds.table::arg::test cvar has value")
test_that("table cvar has value", {
  res <- ds.table(rvar="D$LAB_TSC", cvar=30)
  
  expect_equal(res, "Error: if cvar is not null, it must have a value which is a character string naming the column variable for the table", fixed=TRUE)
})

# context("ds.table::arg::test cvar is not a variable")
test_that("table cvar is not a variable", {
    expect_error(ds.table(rvar="D$DIS_CVA", cvar="D$NOT_VARIABLE"), "The input object D$NOT_VARIABLE is not defined in sim1, sim2, sim3!", fixed = TRUE)

    res.errors <- DSI::datashield.errors()

    expect_length(res.errors, 0)
})

# context("ds.table::arg::test stvar has value")
test_that("table stvar has value", {
  res <- ds.table(rvar="D$LAB_TSC", cvar=NULL, stvar=25)
  
  expect_equal(res, "Error: if stvar is not null, it must have a value which is a character string naming the variable coding separate tables for the table", fixed=TRUE)
})

# context("ds.table::arg::test stvar is not a variable")
test_that("table stvar is not a variable", {
    expect_error(ds.table(rvar="D$DIS_CVA", cvar="D$DIS_AMI", stvar="D$NOT_VARIABLE"), "The input object D$NOT_VARIABLE is not defined in sim1, sim2, sim3!", fixed = TRUE)

    res.errors <- DSI::datashield.errors()

    expect_length(res.errors, 0)
})

# context("ds.table::arg::test stvar has CORRECT value")
test_that("table useNA has value", {
  expect_error(ds.table(rvar="D$LAB_TSC", cvar=NULL, stvar=NULL, useNA = c("BEN")), "useNA must be either 'no' or 'always'.", fixed = TRUE)
})

# context("ds.table::arg::test force.nfilter has value")
test_that("table force.nfilter has value", {
  expect_error(ds.table(rvar="D$LAB_TSC", cvar=NULL, stvar=NULL, useNA = c("ifany"), force.nfilter =30), "useNA must be either 'no' or 'always'.", fixed = TRUE)
})


#
# Done
#

disconnect.studies.dataset.cnsim()
