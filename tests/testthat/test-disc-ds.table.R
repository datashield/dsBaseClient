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
###reminder: how to actually run a test: devtools::test(filter="smk-ds.table$")

#
# Set up - Phase 1
#

# context("ds.table::disc::setup::phase 1")

connect.all.datasets()

test_that("setup", {
  ds_expect_variables(c("D"))
})

#
# Tests - Phase 1
#

# context("ds.table::disc::tests::phase 1::table.assign=FALSE")
test_that("table disclosure table.assign=FALSE", {
  ds.asFactor(input.var.name="D$FACTOR_CHARACTER", newobj.name="factorCharacter")
  ds.asFactor(input.var.name="D$FACTOR_INTEGER", newobj.name="factorInteger")
  myvectors <- c('factorCharacter', 'factorInteger')
  ds.dataFrame(x=myvectors, newobj="tablesource")

  table.res <- ds.table(rvar='tablesource$factorCharacter', cvar='tablesource$factorInteger', table.assign=FALSE, newobj="new_table1")

  expect_length(table.res, 2)
  expect_length(table.res$output.list, 9)
  expect_length(table.res$validity.message, 4)
  expect_equal(table.res$validity.message[1], "At least one study failed for reasons identified by 'error.messages':", fixed = TRUE)
  expect_equal(table.res$validity.message[2], "Study1: No errors reported from this study", fixed = TRUE)
  expect_equal(table.res$validity.message[3], "Study2: Failed: at least one cell has a non-zero count less than nfilter.tab i.e. 3", fixed = TRUE)
  expect_equal(table.res$validity.message[4], "Study3: No errors reported from this study", fixed = TRUE)
})

# context("ds.table::disc::tests::phase 1::table.assign=TRUE")
test_that("table disclosure table.assign=TRUE", {
  ds.asFactor(input.var.name="D$FACTOR_CHARACTER", newobj.name="factorCharacter")
  ds.asFactor(input.var.name="D$FACTOR_INTEGER", newobj.name="factorInteger")
  myvectors <- c('factorCharacter', 'factorInteger')
  ds.dataFrame(x=myvectors, newobj="tablesource")
  
  table.res <- ds.table(rvar='tablesource$factorCharacter', cvar='tablesource$factorInteger', table.assign=TRUE, newobj="new_table2")
  
  expect_length(table.res, 0)
})

# context("ds.table::disc::test rvar is big numeric")
test_that("table rvar is big numeric", {
    expect_error(ds.table(rvar="D$NUMERIC"), "There are some DataSHIELD errors, list them with datashield.errors()", fixed = TRUE)

    res.errors <- DSI::datashield.errors()

    expect_length(res.errors, 3)
    expect_match(res.errors$study1, "* Error : FAILED: this variable has too many levels and may be disclosive. It exceeds the max number of levels allowed by nfilter.levels.max: that is 40. In this study this variable has 71 factor levels")
    expect_match(res.errors$study2, "* Error : FAILED: this variable has too many levels and may be disclosive. It exceeds the max number of levels allowed by nfilter.levels.max: that is 40. In this study this variable has 71 factor levels")
    expect_match(res.errors$study3, "* Error : FAILED: this variable has too many levels and may be disclosive. It exceeds the max number of levels allowed by nfilter.levels.max: that is 40. In this study this variable has 71 factor levels")
})

#
# Shutdown - Phase 1
#

# context("ds.table::disc::shutdown::phase 1")

test_that("setup", {
  ds_expect_variables(c("D", "factorCharacter", "factorInteger", "tablesource", "new_table2"))
})

disconnect.all.datasets()

#
# Set up - Phase 2
#

# context("ds.table::disc::setup::phase 2")

connect.studies.dataset.cnsim(list("GENDER", "DIS_AMI"))

test_that("setup", {
  ds_expect_variables(c("D"))
})

#
# Tests - Phase 2
#

# context("ds.table::disc::tests::phase 2::table.assign=FALSE")
test_that("table disclosure, table.assign=FALSE", {
  table.res <- ds.table(rvar='D$GENDER', cvar="D$DIS_AMI", table.assign=FALSE, newobj="new_table3")
  
  expect_length(table.res, 2)
  expect_length(table.res$validity.message, 1)
  expect_equal(table.res$validity.message, "All studies failed for reasons identified below", fixed = TRUE)
  expect_length(table.res$error.message, 3)
  expect_equal(table.res$error.message$sim1, "Failed: at least one cell has a non-zero count less than nfilter.tab i.e. 3", fixed = TRUE)
  expect_equal(table.res$error.message$sim2, "Failed: at least one cell has a non-zero count less than nfilter.tab i.e. 3", fixed = TRUE)
  expect_equal(table.res$error.message$sim3, "Failed: at least one cell has a non-zero count less than nfilter.tab i.e. 3", fixed = TRUE)
})

# context("ds.table::disc::tests::phase 2::table.assign=TRUE")
test_that("table disclosure, table.assign=TRUE", {
  table.res <- ds.table(rvar='D$GENDER', cvar="D$DIS_AMI", table.assign=TRUE, newobj="new_table4")
  
  expect_length(table.res, 0)
})

#
# Shutdown - Phase 2
#

# context("ds.table::disc::shutdown::phase 2")

test_that("setup", {
  ds_expect_variables(c("D", "new_table4"))
})

disconnect.studies.dataset.cnsim()

#
# Done
#

# context("ds.table::disc::done")
