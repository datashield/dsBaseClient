#-------------------------------------------------------------------------------
# Copyright (c) 2019-2022 University of Newcastle upon Tyne. All rights reserved.
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

# context("ds.table2D::smk::setup")

connect.studies.dataset.cnsim(list("DIS_DIAB", "GENDER"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.table2D::smk")
test_that("simple table2D", {
    table2D.res <- expect_warning(ds.table2D(x='D$DIS_DIAB', y='D$GENDER'), "'ds.table2D' is deprecated.\nUse 'ds.table' instead.", fixed = TRUE)

    expect_length(table2D.res, 9)
    expect_length(table2D.res$colPercent, 3)
    expect_length(table2D.res$colPercent$`sim1-D`, 3)
    expect_length(table2D.res$colPercent$`sim2-D`, 3)
    expect_length(table2D.res$colPercent$`sim3-D`, 3)
    expect_length(table2D.res$colPercent.all.studies, 1)
    expect_length(table2D.res$rowPercent, 3)
    expect_length(table2D.res$rowPercent$`sim1-D`, 3)
    expect_length(table2D.res$rowPercent$`sim2-D`, 3)
    expect_length(table2D.res$rowPercent$`sim3-D`, 3)
    expect_length(table2D.res$rowPercent.all.studies, 1)
    expect_length(table2D.res$chi2Test, 3)
    expect_length(table2D.res$chi2Test$`sim1-D`, 9)
    expect_length(table2D.res$chi2Test$`sim2-D`, 9)
    expect_length(table2D.res$chi2Test$`sim3-D`, 9)
    expect_length(table2D.res$chi2Test.all.studies, 1)
    expect_length(table2D.res$counts, 3)
    expect_length(table2D.res$counts$`sim1-D`, 3)
    expect_length(table2D.res$counts$`sim2-D`, 3)
    expect_length(table2D.res$counts$`sim3-D`, 3)
    expect_length(table2D.res$counts.all.studies, 1)
    expect_equal(table2D.res$validity, "All tables are valid!", fixed=TRUE)
})

#
# Done
#

# context("ds.table2D::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D"))
})

disconnect.studies.dataset.cnsim()

# context("ds.table2D::smk::done")
