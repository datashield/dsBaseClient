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

# context("ds.cbind::discctrl::setup")

connect.studies.dataset.cnsim(list("GENDER", "PM_BMI_CATEGORICAL"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#
# context("ds.cbind::discctrl")
test_that("simple c", {
    expect_error(ds.cbind(x="D", DataSHIELD.checks=TRUE, datasources = ds.test_env$connections[1]), "There are some DataSHIELD errors, list them with datashield.errors()", fixed = TRUE)
    
    res_errors <- DSI::datashield.errors()

    expect_length(res_errors, 1)
    expect_length(res_errors$sim1, 1)
    expect_equal(res_errors$sim1, "Command 'cbindDS(\"D\", \"GENDER,PM_BMI_CATEGORICAL\")' failed on 'sim1': Error while evaluating 'is.null(base::assign('cbind.newobj', value={dsBase::cbindDS(\"D\", \"GENDER,PM_BMI_CATEGORICAL\")}))' -> Error in checkPermissivePrivacyControlLevel() : \n  BLOCKED: The server is running in 'non-permissive' mode which has caused this method to be blocked\n", fixed = TRUE)
})

#
# Done
#

# context("ds.cbind::discctrl::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D"))
})

disconnect.studies.dataset.cnsim()

# context("ds.cbind::discctrl::done")
