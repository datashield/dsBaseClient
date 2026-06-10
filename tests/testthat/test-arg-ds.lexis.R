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

connect.studies.dataset.cnsim(list("LAB_TSC"))

#
# Tests
#

# context("ds.lexis::arg::test errors")
test_that("lexis_erros", {
    expect_error(ds.lexis(), "Please provide the name of the column that holds the subject IDs!", fixed=TRUE)
})

# context("ds.lexis::arg::test intervalWidth")
test_that("simple lexis", {
    expect_error(ds.lexis(data='D', intervalWidth = NA, idCol = 'D$id', entryCol = 'D$starttime', exitCol = 'D$endtime', statusCol = 'D$cens', variables = c('D$age.60'), expandDF = 'EM.new'), "Please provide a (non-zero) single numeric value or vector to identify the survival time intervals", fixed = TRUE)
    expect_error(ds.lexis(data='D', intervalWidth = NULL, idCol = 'D$id', entryCol = 'D$starttime', exitCol = 'D$endtime', statusCol = 'D$cens', variables = c('D$age.60'), expandDF = 'EM.new'), "Please provide a (non-zero) single numeric value or vector to identify the survival time intervals", fixed = TRUE)
    expect_error(ds.lexis(data='D', intervalWidth = 0.0, idCol = 'D$id', entryCol = 'D$starttime', exitCol = 'D$endtime', statusCol = 'D$cens', variables = c('D$age.60'), expandDF = 'EM.new'), "Please provide a (non-zero) single numeric value or vector to identify the survival time intervals", fixed = TRUE)
    expect_error(ds.lexis(data='D', intervalWidth = c(1.0, NA, 2.5), idCol = 'D$id', entryCol = 'D$starttime', exitCol = 'D$endtime', statusCol = 'D$cens', variables = c('D$age.60'), expandDF = 'EM.new'), "Please provide a (non-zero) single numeric value or vector to identify the survival time intervals", fixed = TRUE)
    expect_error(ds.lexis(data='D', intervalWidth = c(1.0, NULL.0, 2.5), idCol = 'D$id', entryCol = 'D$starttime', exitCol = 'D$endtime', statusCol = 'D$cens', variables = c('D$age.60'), expandDF = 'EM.new'), "object 'NULL.0' not found", fixed = TRUE)
    expect_error(ds.lexis(data='D', intervalWidth = c(1.0, 0.0, 2.5), idCol = 'D$id', entryCol = 'D$starttime', exitCol = 'D$endtime', statusCol = 'D$cens', variables = c('D$age.60'), expandDF = 'EM.new'), "Please provide a (non-zero) single numeric value or vector to identify the survival time intervals", fixed = TRUE)

#    res <- ds.lexis(data='D', intervalWidth = c(1.0, 1.5, 2.5), idCol = 'D$id', entryCol = 'D$starttime', exitCol = 'D$endtime', statusCol = 'D$cens', variables = c('D$age.60'), expandDF = 'EM.new')

})

#
# Done
#

disconnect.studies.dataset.cnsim()
