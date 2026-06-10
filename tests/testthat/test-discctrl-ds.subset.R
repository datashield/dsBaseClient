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

# context("ds.subset::discctrl::setup")

connect.studies.dataset.cnsim(list("DIS_DIAB","PM_BMI_CONTINUOUS","LAB_HDL", "GENDER"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#
# context("ds.subset::discctrl")
test_that("simple subset", {
    expect_error(expect_warning(ds.subset(datasources=ds.test_env$connections, subset='subD', x='D', rows=c(1:50), cols=c(1,2)), "", fixed = TRUE), "There are some DataSHIELD errors, list them with datashield.errors()", fixed = TRUE)
    
    res_errors <- DSI::datashield.errors()

    expect_length(res_errors, 1)
    expect_length(res_errors$sim1, 1)
    expect_equal(res_errors$sim1, "Command 'subsetDS(\"D\", FALSE, 1:50, c(1, 2))' failed on 'sim1': Error while evaluating 'is.null(base::assign('subD', value={dsBase::subsetDS(\"D\", FALSE, 1:50, dsBase::vectorDS(1, 2))}))' -> Error in checkPermissivePrivacyControlLevel() : \n  BLOCKED: The server is running in 'non-permissive' mode which has caused this method to be blocked\n", fixed = TRUE)
})

#
# Done
#

# context("ds.subset::discctrl::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D"))
})

disconnect.studies.dataset.cnsim()

# context("ds.subset::discctrl::done")
