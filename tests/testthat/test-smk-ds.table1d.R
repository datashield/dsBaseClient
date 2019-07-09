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

context("dsClient::ds.table1d")

options(datashield.variables=list("DIS_CVA","GENDER"))
source("setup.R")

#
# Tests
#

context("dsClient::ds.table1D() generate a one dimensional table, outputting combined contingency tables")
res <- ds.table1D(x='D$GENDER')
#print(res)
test_that("GENDER_normal", {
    expect_equal(res$validity, "All tables are valid!")
    expect_equal(res$counts[2], 4611)
    expect_equal(res$counts[3], 9379)
})


context("dsClient::ds.table1D() generate a one dimensional table, outputting combined contingency tables fail")
res <- ds.table1D(x='D$DIS_CVA')
#print(res)
test_that("DIS_CVA_invalid", {
    expect_equal(res$validity, "Invalid tables from 'sim2'!")
})

context("dsClient::ds.table1D() generate a one dimensional table, outputting combined contingency tables fail split")
res <- ds.table1D(x='D$DIS_CVA', type="split")
#print(res)
test_that("DIS_CVA_invalid_split", {
    expect_equal(res$validity, "Invalid table(s) from 'sim2'!")
})

context("dsClient::ds.table1D() generate a one dimensional table, outputting study specific contingency tables")
res <- ds.table1D(x='D$GENDER', type="split")
#print(res)
test_that("GENDER_split", {
    expect_equal(res$validity, "All tables are valid!")
    expect_equal(res$counts$sim1[1], 1092)
    expect_equal(res$counts$sim2[2], 1503)
    expect_equal(res$counts$sim3[1], 2091)
})

context("dsClient::ds.table1D() generate a one dimensional table, outputting study specific contingency tables for study 1 and 2")
res <- ds.table1D(datasources=opals[1:2], x='D$GENDER', type="split")
#print(res)
test_that("GENDER_split_12", {
    expect_equal(res$validity, "All tables are valid!")
    expect_equal(res$counts$sim1[1], 1092)
    expect_equal(res$counts$sim2[2], 1503)
    expect_equal(res$counts$sim3[1], NULL)
})

context("dsClient::ds.table1D() generate a one dimensional table, outputting study specific and combined contingency tables")
res <- ds.table1D(datasources=opals, x='D$GENDER')
#print(res)
test_that("GENDER_normal_2", {
    expect_equal(res$validity, "All tables are valid!")
    expect_equal(res$counts[2], 4611)
    expect_equal(res$counts[3], 9379)
})

context("dsClient::ds.table1D() test errors")
test_that("table1D_erros", {
    expect_error(ds.table1D(), "Please provide the name of the input vector!", fixed=TRUE)
    expect_error(ds.table1D(x='D$GENDER', type="datashield"), "Function argument 'type' has to be either 'combine' or 'split'", fixed=TRUE)
})
#
# Tear down
#

source("teardown.R")