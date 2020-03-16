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
  ds.asFactor(input.var.name="D$FACTOR_CHARACTER", newobj.name="factorCharacter")
  ds.asFactor(input.var.name="D$FACTOR_INTEGER", newobj.name="factorInteger")
  
  myvectors <- c('factorInteger', 'factorCharacter')
  res <- ds.dataFrame(x=myvectors, newobj="tablesource")
  subset.res <- ds.dataFrameSubset(df.name="tablesource", V1.name="factorInteger", V2.name='6', Boolean.operator="!=", newobj="tablesource_subset")
  
  
  expect_length(subset.res, 2)
  expect_equal(subset.res$is.object.created, "A data object <tablesource_subset> has been created in all specified data sources")
  expect_equal(subset.res$validity.check, "<tablesource_subset> appears valid in all sources")
  
  table.res <- ds.table(rvar='tablesource_subset$factorInteger', cvar='tablesource_subset$factorCharacter', newobj="new_table")
  
  expect_length(table.res, 2)
  expect_length(table.res$output.list, 12)
  expect_equal(class(table.res$output.list[1]), "list")
  expect_equal(class(table.res$output.list[2]), "list")
  expect_equal(class(table.res$output.list[3]), "list")
  expect_equal(class(table.res$output.list[4]), "list")
  expect_equal(class(table.res$output.list[5]), "list")
  expect_equal(class(table.res$output.list[6]), "list")
  expect_equal(class(table.res$output.list[7]), "list")
  expect_equal(class(table.res$output.list[8]), "list")
  expect_equal(class(table.res$output.list[9]), "list")
  expect_equal(class(table.res$output.list[10]), "list")
  expect_equal(class(table.res$output.list[11]), "list")
  expect_equal(class(table.res$output.list[12]), "list")
  expect_length(table.res$validity.message, 1)
  expect_equal(table.res$validity.message, "Data in all studies were valid")
})

#
# Done
#

context("ds.table::smk::shutdown")

test_that("shutdown", {
  ds_expect_variables(c("D", "factorCharacter","factorInteger","tablesource","tablesource_subset"))
})

disconnect.all.datasets()


connect.testing.group.dataset.1()
context("ds.table::smkgrouptest")
test_that("simple table", {
  
  GROUP1 <- read.csv("data_files/GROUP1.csv")
  GROUP2 <- read.csv("data_files/GROUP2_test.csv")
  
  server.result <- ds.table("D$COLOURS", "D$POSITIVE.NUMBERS") ##############it works!!! ###############
  
  expect_length(server.result, 2)
  expect_length(server.result$output.list, 9)
  expect_equal(class(server.result$output.list), "list")
}  
)
disconnect.testing.group.dataset.1()

context("ds.table::smk::done")