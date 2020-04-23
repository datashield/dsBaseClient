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

connect.all.datasets()

#
# Tests
#

context("ds.table::disc")
test_that("table disclosure", {
  ds.asFactor(input.var.name="D$FACTOR_CHARACTER", newobj.name="factorCharacter")
  ds.asFactor(input.var.name="D$FACTOR_INTEGER", newobj.name="factorInteger")
  myvectors <- c('factorCharacter', 'factorInteger')
  ds.dataFrame(x=myvectors, newobj="tablesource")
#  print(ds.ls())
#  print(ds.colnames("tablesource"))
#  table.res <- ds.table(rvar='tablesource$D.FACTOR_INTEGER', cvar='tablesource$D.FACTOR_CHARACTER', newobj="new_table")
  table.res <- ds.table(rvar='tablesource$factorCharacter', cvar='tablesource$factorInteger', newobj="new_table")

  expect_equal(table.res$validity.message,"At least one study failed for reasons identified by 'error.messages'")
  
  expect_length(table.res,2)
#  print("---")
#  print(table.res[1])
#  print(length(table.res[[1]]))
#  print("---") 
  expect_length(table.res[[1]],9)
  ################### check expect length of table.res to be 2....
  
  ### length of output list should be 6
}
)
#
# Done
#
disconnect.all.datasets()
