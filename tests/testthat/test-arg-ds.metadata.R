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

# context("ds.metadata::arg::setup")

connect.studies.dataset.cnsim(list('LAB_TSC', 'LAB_TRIG', 'LAB_HDL', 'LAB_GLUC_ADJUSTED', 'PM_BMI_CONTINUOUS', 'DIS_CVA', 'MEDI_LPD', 'DIS_DIAB', 'DIS_AMI', 'GENDER', 'PM_BMI_CATEGORICAL'))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.metadata::arg")
test_that("missing variable", {
    expect_error(ds.metadata(x='E'), "The input object E is not defined in sim1, sim2, sim3!", fixed = TRUE)
})

# test_that("missing column", {
#     expect_error(ds.metadata(x='E$E'), "The input object E$E is not defined in sim1, sim2, sim3!", fixed = TRUE)
# })

test_that("missing column", {
    expect_error(ds.metadata(x='D$E'), "The input object D$E is not defined in sim1, sim2, sim3!", fixed = TRUE)
})

#
# Tear down
#

# context("ds.metadata::arg::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D"))
})

disconnect.studies.dataset.cnsim()

# context("ds.metadata::arg::done")
