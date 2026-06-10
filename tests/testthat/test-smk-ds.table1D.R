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

# context("ds.table1D::smk::setup")

connect.studies.dataset.cnsim(list("DIS_CVA","GENDER"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.table1D::smk::generate a one dimensional table, outputting combined contingency tables")
test_that("GENDER_normal", {
    res <- expect_warning(ds.table1D(x='D$GENDER'), "'ds.table1D' is deprecated.\nUse 'ds.table' instead.", fixed = TRUE)

    expect_equal(res$validity, "All tables are valid!")
    expect_equal(res$counts[2], 4611)
    expect_equal(res$counts[3], 9379)
})

# context("ds.table1D::smk::generate a one dimensional table, outputting combined contingency tables fail")
test_that("DIS_CVA_invalid", {
    res <- expect_warning(ds.table1D(x='D$DIS_CVA'), "'ds.table1D' is deprecated.\nUse 'ds.table' instead.", fixed = TRUE)

    expect_length(res, 3)
    expect_equal(class(res), "list")
    expect_length(res$counts, 3)
    expect_length(res$percentages, 3)
    expect_length(res$validity, 1)
    expect_equal(res$validity, "All tables are valid!")
})

# context("ds.table1D::smk::generate a one dimensional table, outputting combined contingency tables fail split")
test_that("DIS_CVA_invalid_split", {
    res <- expect_warning(ds.table1D(x='D$DIS_CVA', type="split"), "'ds.table1D' is deprecated.\nUse 'ds.table' instead.", fixed = TRUE)

    expect_length(res, 3)
    expect_equal(class(res), "list")
    expect_length(res$counts, 3)
    expect_length(res$percentages, 3)
    expect_length(res$validity, 1)
    expect_equal(res$validity, "All tables are valid!")
})

# context("ds.table1D::smk::generate a one dimensional table, outputting study specific contingency tables")
test_that("GENDER_split", {
    res <- expect_warning(ds.table1D(x='D$GENDER', type="split"), "'ds.table1D' is deprecated.\nUse 'ds.table' instead.", fixed = TRUE)

    expect_equal(res$validity, "All tables are valid!")
    expect_equal(res$counts$sim1[1], 1092)
    expect_equal(res$counts$sim2[2], 1503)
    expect_equal(res$counts$sim3[1], 2091)
})

# context("ds.table1D::smk::generate a one dimensional table, outputting study specific contingency tables for study 1 and 2")
test_that("GENDER_split_12", {
    res <- expect_warning(ds.table1D(datasources=ds.test_env$connections[1:2], x='D$GENDER', type="split"), "'ds.table1D' is deprecated.\nUse 'ds.table' instead.", fixed = TRUE)

    expect_equal(res$validity, "All tables are valid!")
    expect_equal(res$counts$sim1[1], 1092)
    expect_equal(res$counts$sim2[2], 1503)
    expect_equal(res$counts$sim3[1], NULL)
})

# context("ds.table1D::smk::generate a one dimensional table, outputting study specific and combined contingency tables")
test_that("GENDER_normal_2", {
    res <- expect_warning(ds.table1D(datasources=ds.test_env$connections, x='D$GENDER'), "'ds.table1D' is deprecated.\nUse 'ds.table' instead.", fixed = TRUE)

    expect_equal(res$validity, "All tables are valid!")
    expect_equal(res$counts[2], 4611)
    expect_equal(res$counts[3], 9379)
})

#
# Tear down
#

# context("ds.table1D::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D"))
})

disconnect.studies.dataset.cnsim()

# context("ds.table1D::smk::done")
