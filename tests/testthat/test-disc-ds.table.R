#-------------------------------------------------------------------------------
# Copyright (c) 2018-2020 University of Newcastle upon Tyne. All rights reserved.
#
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------
###reminder: how to actually run a test: devtools::test(filter="smk-ds.table$")

#
# Set up
#

context("ds.table::disc::setup")

connect.all.datasets()

#
# Tests
#

context("ds.table::disc::tests")
test_that("table disclosure", {
  ds.asFactor(input.var.name="D$FACTOR_CHARACTER", newobj.name="factorCharacter")
  ds.asFactor(input.var.name="D$FACTOR_INTEGER", newobj.name="factorInteger")
  myvectors <- c('factorCharacter', 'factorInteger')
  ds.dataFrame(x=myvectors, newobj="tablesource")

  table.res <- ds.table(rvar='tablesource$factorCharacter', cvar='tablesource$factorInteger', newobj="new_table")

  expect_length(table.res, 2)
  expect_length(table.res$output.list, 9)
  expect_length(table.res$validity.message, 4)
  expect_equal(table.res$validity.message[1], "At least one study failed for reasons identified by 'error.messages':", fixed = TRUE)
  expect_equal(table.res$validity.message[2], "Study1: No errors reported from this study", fixed = TRUE)
  expect_equal(table.res$validity.message[3], "Study2: Failed: at least one cell has a non-zero count less than nfilter.tab i.e. 3", fixed = TRUE)
  expect_equal(table.res$validity.message[4], "Study3: No errors reported from this study", fixed = TRUE)
})

#
# Shutdown
#

context("ds.table::disc::shutdown")

disconnect.all.datasets()

#
# Done
#

context("ds.table::disc::done")
