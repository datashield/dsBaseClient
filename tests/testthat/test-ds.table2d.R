#-------------------------------------------------------------------------------
# Copyright (c) 2014 OBiBa. All rights reserved.
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

context("dsClient::ds.table2D")

options(datashield.variables=list("DIS_DIAB", "DIS_CVA", "GENDER", "LAB_HDL"))
source("setup.R")

#
# Tests
#

context("dsClient::ds.table2D() generate a two dimensional table, outputting combined contingency tables - default behaviour")
res <- ds.table2D(datasources=opals, x='D$DIS_DIAB', y='D$GENDER')
#print(res)
test_that("DIS_DIAB_GENDER", {
    expect_equal(res$validity, "All tables are valid!")
    expect_equal(res$chi2Test$`pooled-D$DIS_DIAB(row)|D$GENDER(col)`$statistic[['X-squared']] , 13.8164197919235, tolerance = .0000000000001)
    expect_equal(res$counts$`pooled-D$DIS_DIAB(row)|D$GENDER(col)`[[1]][[1]], 4671)
    expect_equal(res$counts$`pooled-D$DIS_DIAB(row)|D$GENDER(col)`[[2]][[2]], 49)
})

context("dsClient::ds.table2D() generate a two dimensional table, outputting study specific contingency tables")
res <- ds.table2D(datasources=opals, x='D$DIS_DIAB', y='D$GENDER', type="split")
#print(res)
test_that("DIS_DIAB_GENDER_split", {
    expect_equal(res$validity, "All tables are valid!")
    expect_equal(res$chi2Test$`sim1-D$DIS_DIAB(row)|D$GENDER(col)`$statistic[['X-squared']], 3.87669757947898, tolerance = .0000000000001)
    expect_equal(res$chi2Test$`sim2-D$DIS_DIAB(row)|D$GENDER(col)`$statistic[['X-squared']], 3.51578318253754, tolerance = .0000000000001)
    expect_equal(res$chi2Test$`sim3-D$DIS_DIAB(row)|D$GENDER(col)`$statistic[['X-squared']], 5.3764253415857, tolerance = .0000000000001)
    expect_equal(res$counts$`sim3-D$DIS_DIAB(row)|D$GENDER(col)`[[1]][[1]], 2046)
    expect_equal(res$counts$`sim2-D$DIS_DIAB(row)|D$GENDER(col)`[[2]][[2]], 16)
})

context("dsClient::ds.table2D() generate a two dimensional table, outputting study specific contingency tables for the first two studies")
res <- ds.table2D(datasources=opals[1:2], 'D$DIS_DIAB', 'D$GENDER', type="split")
#print(res)
test_that("DIS_DIAB_GENDER_split_12", {
    expect_equal(res$validity, "All tables are valid!")
    expect_equal(res$chi2Test$`sim1-D$DIS_DIAB(row)|D$GENDER(col)`$statistic[['X-squared']], 3.87669757947898, tolerance = .0000000000001)
    expect_equal(res$chi2Test$`sim2-D$DIS_DIAB(row)|D$GENDER(col)`$statistic[['X-squared']], 3.51578318253754, tolerance = .0000000000001)
    expect_equal(res$chi2Test$`sim3-D$DIS_DIAB(row)|D$GENDER(col)`$statistic[['X-squared']], NULL)
    expect_equal(res$counts$`sim3-D$DIS_DIAB(row)|D$GENDER(col)`[[1]][[1]], NULL)
    expect_equal(res$counts$`sim2-D$DIS_DIAB(row)|D$GENDER(col)`[[2]][[2]], 16)
})

context("dsClient::ds.table2D() generate a two dimensional table, outputting combined contingency tables (in this case some studies are invalid)")
res <- ds.table2D(datasources=opals, 'D$DIS_CVA', 'D$GENDER')
#print(res)
test_that("DIS_CVA_GENDER_split_invalid", {
    expect_equal(res$validity, "Invalid contingency table from 'sim2, sim3'!")
    expect_true(is.na(res$counts$`pooled-D$DIS_CVA(row)|D$GENDER(col)`[[1]][[1]]))
    expect_true(is.na(res$colPercent$`pooled-D$DIS_CVA(row)|D$GENDER(col)`[[2]][[2]]))
})

context("dsClient::ds.table2D() generate a two dimensional table, outputting study specific contingency tables (in this case some studies are invalid)")
res <- ds.table2D('D$DIS_CVA', 'D$GENDER', type="split")
#print(res)
test_that("DIS_CVA_GENDER_split_invalid_split", {
    expect_equal(res$validity, "Invalid contingency table from 'sim2, sim3'!")
    expect_false(is.na(res$rowPercent$`sim1-D$DIS_CVA(row)|D$GENDER(col)`[[1]][[1]]))
    expect_true(is.na(res$rowPercent$`sim1-D$DIS_CVA(row)|D$GENDER(col)`[[2]][[2]]))
    expect_true(is.na(res$rowPercent$`sim2-D$DIS_CVA(row)|D$GENDER(col)`[[1]][[1]]))
    expect_true(is.na(res$colPercent$`sim3-D$DIS_CVA(row)|D$GENDER(col)`[[2]][[2]]))
    expect_true(is.na(res$colPercent$`sim3-D$DIS_CVA(row)|D$GENDER(col)`[[1]][[2]]))
})

context("dsClient::ds.table2D() test errors")
test_that("table2D_erros", {
    expect_error(ds.table2D(), "Please provide the name of the input vector!", fixed=TRUE)
    expect_error(ds.table2D('D$DIS_CVA', 'D$GENDER', type="datashield"), "Function argument 'type' has to be either 'combine' or 'split'", fixed=TRUE)
})

#
# Tear down
#

source("teardown.R")