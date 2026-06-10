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

# context("ds.lexis::smk::setup")

connect.studies.dataset.survival(list("id", "starttime", "endtime", "cens", "age.60"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.lexis::smk")
test_that("simple lexis", {
    res <- ds.lexis(data='D', intervalWidth = c(1.0, 1.5, 2.5), idCol = 'D$id', entryCol = 'D$starttime', exitCol = 'D$endtime', statusCol = 'D$cens', variables = c('D$age.60'), expandDF = 'EM.new')

    expect_length(res, 5)
    expect_equal(res$maxmaxtime, 10.25, tolerance=0.5)
    expect_equal(res$Note1, "END OF LAST FOLLOW-UP PERIOD SET (RANDOMLY) AT maxmaxtime:")
    expect_equal(res$Note2, "ASSIGN FUNCTION COMPLETED - USE ds.ls() TO CONFIRM")
    expect_equal(res$Note3, "IF FUNCTION FAILED ON ONE OR MORE STUDIES WITHOUT EXPLANATION, TYPE [PRECISELY] THE COMMAND:")
    expect_equal(res$Note4, "ds.message('messageobj') FOR MORE ERROR MESSAGES")

    res.exists <- ds.exists('EM.new')

    expect_length(res.exists, 3)
    expect_true(res.exists$survival1)
    expect_true(res.exists$survival2)
    expect_true(res.exists$survival3)

    res.message <- expect_warning(ds.message('messageobj'), "'ds.message' is deprecated.", fixed = TRUE)

    expect_length(res.message, 3)
    expect_equal(res.message$survival1, "ALL OK: there are no studysideMessage(s) on this datasource")
    expect_equal(res.message$survival2, "ALL OK: there are no studysideMessage(s) on this datasource")
    expect_equal(res.message$survival3, "ALL OK: there are no studysideMessage(s) on this datasource")
})

#
# Done
#

# context("ds.lexis::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "EM.new", "messageobj"))
})

disconnect.studies.dataset.survival()

# context("ds.lexis::smk::done")
