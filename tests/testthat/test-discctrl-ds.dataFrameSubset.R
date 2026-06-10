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

# context("ds.dataFrameSubset::discctrl::setup")

connect.studies.dataset.cnsim(list("GENDER", "PM_BMI_CATEGORICAL"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#
# context("ds.dataFrameSubset::discctrl")
test_that("simple c", {
    expect_error(ds.dataFrameSubset(df.name="D", V1.name="D$LAB_TSC", V2.name="D$LAB_HDL", Boolean.operator="!=", newobj="subset_df"), "There are some DataSHIELD errors, list them with datashield.errors()", fixed = TRUE)
    
    res_errors <- DSI::datashield.errors()

    expect_length(res_errors, 3)
    expect_length(res_errors$sim1, 1)
    expect_equal(res_errors$sim1, "Command 'dataFrameSubsetDS1(\"D\", \"D$LAB_TSC\", \"D$LAB_HDL\", 2, NULL, NULL, \n    FALSE)' failed on 'sim1': Error while evaluating 'dsBase::dataFrameSubsetDS1(\"D\", \"D$LAB_TSC\", \"D$LAB_HDL\", 2, NULL, NULL, FALSE)' -> Error in checkPermissivePrivacyControlLevel() : \n  BLOCKED: The server is running in 'non-permissive' mode which has caused this method to be blocked\n", fixed = TRUE)
    expect_length(res_errors$sim2, 1)
    expect_equal(res_errors$sim2, "Command 'dataFrameSubsetDS1(\"D\", \"D$LAB_TSC\", \"D$LAB_HDL\", 2, NULL, NULL, \n    FALSE)' failed on 'sim2': Error while evaluating 'dsBase::dataFrameSubsetDS1(\"D\", \"D$LAB_TSC\", \"D$LAB_HDL\", 2, NULL, NULL, FALSE)' -> Error in checkPermissivePrivacyControlLevel() : \n  BLOCKED: The server is running in 'non-permissive' mode which has caused this method to be blocked\n", fixed = TRUE)
    expect_length(res_errors$sim3, 1)
    expect_equal(res_errors$sim3, "Command 'dataFrameSubsetDS1(\"D\", \"D$LAB_TSC\", \"D$LAB_HDL\", 2, NULL, NULL, \n    FALSE)' failed on 'sim3': Error while evaluating 'dsBase::dataFrameSubsetDS1(\"D\", \"D$LAB_TSC\", \"D$LAB_HDL\", 2, NULL, NULL, FALSE)' -> Error in checkPermissivePrivacyControlLevel() : \n  BLOCKED: The server is running in 'non-permissive' mode which has caused this method to be blocked\n", fixed = TRUE)
})

#
# Done
#

# context("ds.dataFrameSubset::discctrl::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D"))
})

disconnect.studies.dataset.cnsim()

# context("ds.dataFrameSubset::discctrl::done")
