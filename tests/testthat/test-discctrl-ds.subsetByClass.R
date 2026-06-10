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

# context("ds.subsetByClass::discctrl::setup")

connect.studies.dataset.cnsim(list("LAB_TSC", "GENDER"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#
# context("ds.subsetByClass::discctrl")
test_that("simple subsetByClass", {
    expect_error(expect_warning(ds.subsetByClass(x='D', subsets='subclasses1'), "", fixed = TRUE), "There are some DataSHIELD errors, list them with datashield.errors()", fixed = TRUE)
    
    res_errors <- DSI::datashield.errors()

    expect_length(res_errors, 3)
    expect_length(res_errors$sim1, 1)
    expect_equal(res_errors$sim1, "Command 'subsetByClassDS('D')' failed on 'sim1': Error while evaluating 'is.null(base::assign('subclasses1', value={dsBase::subsetByClassDS('D')}))' -> Error in checkPermissivePrivacyControlLevel() : \n  BLOCKED: The server is running in 'non-permissive' mode which has caused this method to be blocked\n", fixed = TRUE)
    expect_length(res_errors$sim2, 1)
    expect_equal(res_errors$sim2, "Command 'subsetByClassDS('D')' failed on 'sim2': Error while evaluating 'is.null(base::assign('subclasses1', value={dsBase::subsetByClassDS('D')}))' -> Error in checkPermissivePrivacyControlLevel() : \n  BLOCKED: The server is running in 'non-permissive' mode which has caused this method to be blocked\n", fixed = TRUE)
    expect_length(res_errors$sim3, 1)
    expect_equal(res_errors$sim3, "Command 'subsetByClassDS('D')' failed on 'sim3': Error while evaluating 'is.null(base::assign('subclasses1', value={dsBase::subsetByClassDS('D')}))' -> Error in checkPermissivePrivacyControlLevel() : \n  BLOCKED: The server is running in 'non-permissive' mode which has caused this method to be blocked\n", fixed = TRUE)
})

#
# Done
#

# context("ds.subsetByClass::discctrl::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D"))
})

disconnect.studies.dataset.cnsim()

# context("ds.subsetByClass::discctrl::done")
