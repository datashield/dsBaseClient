#-------------------------------------------------------------------------------
# Copyright (c) 2019-2022 University of Newcastle upon Tyne. All rights reserved.
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

context("ds.table::smk::dataframe setup")
test_that("simple dataframe input setup", {
  ds.asFactor(input.var.name="D$FACTOR_CHARACTER", newobj.name="factorCharacter")
  ds.asFactor(input.var.name="D$FACTOR_INTEGER", newobj.name="factorInteger")
  ds.asFactor(input.var.name="D$CATEGORY", newobj.name="factorCategory")

  myvectors <- c('factorInteger', 'factorCharacter', "factorCategory")
  res1 <- ds.dataFrame(x=myvectors, newobj="tablesource")
  subset.res <- ds.dataFrameSubset(df.name="tablesource", V1.name="factorInteger", V2.name='6', Boolean.operator="!=", newobj="tablesource_subset")

  expect_length(subset.res, 2)
  expect_equal(subset.res$is.object.created, "A data object <tablesource_subset> has been created in all specified data sources")
  expect_equal(subset.res$validity.check, "<tablesource_subset> appears valid in all sources")
})

context("ds.table::smk::simple table 1D")
test_that("simple table 1D", {
  table1.res <- ds.table(rvar='tablesource_subset$factorCharacter')

  expect_length(table1.res, 2)
  expect_length(table1.res$output.list, 5)
  expect_equal(class(table1.res$output.list[1]), "list")
  expect_equal(class(table1.res$output.list[2]), "list")
  expect_equal(class(table1.res$output.list[3]), "list")
  expect_equal(class(table1.res$output.list[4]), "list")
  expect_equal(class(table1.res$output.list[5]), "list")
  expect_length(table1.res$validity.message, 1)
  expect_equal(table1.res$validity.message, "Data in all studies were valid")
})

context("ds.table::smk::simple table 2D")
test_that("simple table 2D", {
  table1.res <- ds.table(rvar='tablesource_subset$factorInteger', cvar='tablesource_subset$factorCharacter')

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

context("ds.table::smk::simple table 3D")
test_that("simple table 3D", {
  expect_error(ds.table(rvar='tablesource_subset$factorInteger', cvar='tablesource_subset$factorCharacter', stvar='tablesource_subset$factorCategory'), "There are some DataSHIELD errors, list them with datashield.errors()", fixed = TRUE)

  res.errors <- DSI::datashield.errors()
  
  expect_length(res.errors, 2)

  expect_match(res.errors$study1, "* Error : Failed: at least one cell has a non-zero count less than nfilter.tab i.e. 3*")
  expect_match(res.errors$study2, "* Error : Failed: at least one cell has a non-zero count less than nfilter.tab i.e. 3*")
})

test_that("simple table 1D, with assign", {
  table.res <- ds.table(rvar='tablesource_subset$factorInteger', newobj="new_table1", table.assign=TRUE)

  expect_length(table.res, 0)

  table.length <- ds.length("new_table1")
  expect_length(table.length, 4)
  expect_equal(table.length$`length of new_table1 in study1`, 4)
  expect_equal(table.length$`length of new_table1 in study2`, 4)
  expect_equal(table.length$`length of new_table1 in study3`, 4)
  expect_equal(table.length$`total length of new_table1 in all studies combined`, 12)

  table.class <- ds.class("new_table1")
  expect_length(table.class, 3)
  expect_equal(table.class$study1, 'list')
  expect_equal(table.class$study2, 'list')
  expect_equal(table.class$study3, 'list')
})

test_that("simple table 2D, with assign", {
  table.res <- ds.table(rvar='tablesource_subset$factorInteger', cvar='tablesource_subset$factorCharacter', newobj="new_table2", table.assign=TRUE)

  expect_length(table.res, 0)

  table.length <- ds.length("new_table2")
  expect_length(table.length, 4)
  expect_equal(table.length$`length of new_table2 in study1`, 4)
  expect_equal(table.length$`length of new_table2 in study2`, 4)
  expect_equal(table.length$`length of new_table2 in study3`, 4)
  expect_equal(table.length$`total length of new_table2 in all studies combined`, 12)

  table.class <- ds.class("new_table2")
  expect_length(table.class, 3)
  expect_equal(table.class$study1, 'list')
  expect_equal(table.class$study2, 'list')
  expect_equal(table.class$study3, 'list')
})

test_that("simple table 3D, with assign", {

  expect_error(ds.table(rvar='tablesource_subset$factorInteger', cvar='tablesource_subset$factorCharacter', stvar='tablesource_subset$factorCategory', newobj="new_table3", table.assign=TRUE), "There are some DataSHIELD errors, list them with datashield.errors()", fixed = TRUE)

  res.errors <- DSI::datashield.errors()
  
  expect_length(res.errors, 2)

  expect_match(res.errors$study1, "* Error : Failed: at least one cell has a non-zero count less than nfilter.tab i.e. 3*")
  expect_match(res.errors$study2, "* Error : Failed: at least one cell has a non-zero count less than nfilter.tab i.e. 3*")
})

#
# Done
#

context("ds.table::smk::shutdown")

test_that("shutdown", {
  ds_expect_variables(c("D", "factorCharacter", "factorInteger", "factorCategory", "tablesource", "tablesource_subset", "new_table1", "new_table2", "new_table3"))
})

disconnect.all.datasets()

context("ds.table::smk::done")
