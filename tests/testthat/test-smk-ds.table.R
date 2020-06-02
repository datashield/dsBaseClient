#-------------------------------------------------------------------------------
# Copyright (c) 2019-2020 University of Newcastle upon Tyne. All rights reserved.
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
context("ds.table::smk::setup")

connect.all.datasets()

test_that("setup", {
  ds_expect_variables(c("D"))
})

#
# Tests
#

context("ds.table::smk")
test_that("simple table", {
  ds.asFactor(input.var.name="D$FACTOR_CHARACTER", newobj.name="factorCharacter1")
  ds.asFactor(input.var.name="D$FACTOR_INTEGER", newobj.name="factorInteger1")
  
  myvectors <- c('factorInteger1', 'factorCharacter1')
  res1 <- ds.dataFrame(x=myvectors, newobj="tablesource1")
  subset.res <- ds.dataFrameSubset(df.name="tablesource1", V1.name="factorInteger1", V2.name='6', Boolean.operator="!=", newobj="tablesource_subset1")

  expect_length(subset.res, 2)
  expect_equal(subset.res$is.object.created, "A data object <tablesource_subset1> has been created in all specified data sources")
  expect_equal(subset.res$validity.check, "<tablesource_subset1> appears valid in all sources")
  
  table1.res <- ds.table(rvar='tablesource_subset1$factorInteger1', cvar='tablesource_subset1$factorCharacter1', newobj="new_table1")
  
  expect_length(table1.res, 2)
  expect_length(table1.res$output.list, 12)
  expect_equal(class(table1.res$output.list[1]), "list")
  expect_equal(class(table1.res$output.list[2]), "list")
  expect_equal(class(table1.res$output.list[3]), "list")
  expect_equal(class(table1.res$output.list[4]), "list")
  expect_equal(class(table1.res$output.list[5]), "list")
  expect_equal(class(table1.res$output.list[6]), "list")
  expect_equal(class(table1.res$output.list[7]), "list")
  expect_equal(class(table1.res$output.list[8]), "list")
  expect_equal(class(table1.res$output.list[9]), "list")
  expect_equal(class(table1.res$output.list[10]), "list")
  expect_equal(class(table1.res$output.list[11]), "list")
  expect_equal(class(table1.res$output.list[12]), "list")
  expect_length(table1.res$validity.message, 1)
  expect_equal(table1.res$validity.message, "Data in all studies were valid")
})

test_that("simple table, with assign", {
  ds.asFactor(input.var.name="D$FACTOR_CHARACTER", newobj.name="factorCharacter2")
  ds.asFactor(input.var.name="D$FACTOR_INTEGER", newobj.name="factorInteger2")
  
  myvectors <- c('factorInteger2', 'factorCharacter2')
  res <- ds.dataFrame(x=myvectors, newobj="tablesource2")
  subset.res <- ds.dataFrameSubset(df.name="tablesource2", V1.name="factorInteger2", V2.name='6', Boolean.operator="!=", newobj="tablesource_subset2")
  
  expect_length(subset.res, 2)
  expect_equal(subset.res$is.object.created, "A data object <tablesource_subset2> has been created in all specified data sources")
  expect_equal(subset.res$validity.check, "<tablesource_subset2> appears valid in all sources")
  
  table2.res <- ds.table(rvar='tablesource_subset2$factorInteger2', cvar='tablesource_subset2$factorCharacter2', newobj="new_table2", table.assign=TRUE)
  
  expect_length(table2.res, 0)
  
  table2.length <- ds.length("new_table2")
  expect_length(table2.length, 4)
  expect_equal(table2.length$`length of new_table2 in study1`, 4)
  expect_equal(table2.length$`length of new_table2 in study2`, 4)
  expect_equal(table2.length$`length of new_table2 in study3`, 4)
  expect_equal(table2.length$`total length of new_table2 in all studies combined`, 12)

  table2.class <- ds.class("new_table2")
  expect_length(table2.class, 3)
  expect_equal(table2.class$study1, 'list')
  expect_equal(table2.class$study2, 'list')
  expect_equal(table2.class$study3, 'list')
})

#
# Done
#

context("ds.table::smk::shutdown")

test_that("shutdown", {
  ds_expect_variables(c("D", "factorCharacter1", "factorCharacter2", "factorInteger1", "factorInteger2", "new_table2", "tablesource1", "tablesource2", "tablesource_subset1", "tablesource_subset2"))
})

disconnect.all.datasets()

context("ds.table::smk::done")
