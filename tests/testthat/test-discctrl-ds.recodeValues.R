#-------------------------------------------------------------------------------
# Copyright (c) 2019-2022 University of Newcastle upon Tyne. All rights reserved.
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

context("ds.rep::discctrl::setup")

connect.studies.dataset.survival(list("survtime", "time.id", "female", "age.60"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#
context("ds.rep::discctrl")
test_that("simple recodeValues", {
    expect_error(ds.recodeValues("D$survtime", values2replace.vector=c(0,1), new.values.vector=c(-10,10), newobj="recodevalues_newobj"), "There are some DataSHIELD errors, list them with datashield.errors()", fixed = TRUE)
    
    res_errors <- DSI::datashield.errors()
    
    expect_length(res_errors, 3)
    expect_length(res_errors$survival1, 1)
    expect_equal(res_errors$survival1, "Command 'recodeValuesDS(\"D$survtime\", \"0,1\", \"-10,10\", NULL)' failed on 'survival1': Error while evaluating 'is.null(base::assign('recodevalues_newobj', value={dsBase::recodeValuesDS(\"D$survtime\", \"0,1\", \"-10,10\", NULL)}))' -> Error in checkPermissivePrivacyControlLevel() : \n  BLOCKED: The server is running in 'non-permissive' mode which has caused this method to be blocked\n", fixed = TRUE)
    expect_length(res_errors$survival2, 1)
    expect_equal(res_errors$survival2, "Command 'recodeValuesDS(\"D$survtime\", \"0,1\", \"-10,10\", NULL)' failed on 'survival2': Error while evaluating 'is.null(base::assign('recodevalues_newobj', value={dsBase::recodeValuesDS(\"D$survtime\", \"0,1\", \"-10,10\", NULL)}))' -> Error in checkPermissivePrivacyControlLevel() : \n  BLOCKED: The server is running in 'non-permissive' mode which has caused this method to be blocked\n", fixed = TRUE)
    expect_length(res_errors$survival3, 1)
    expect_equal(res_errors$survival3, "Command 'recodeValuesDS(\"D$survtime\", \"0,1\", \"-10,10\", NULL)' failed on 'survival3': Error while evaluating 'is.null(base::assign('recodevalues_newobj', value={dsBase::recodeValuesDS(\"D$survtime\", \"0,1\", \"-10,10\", NULL)}))' -> Error in checkPermissivePrivacyControlLevel() : \n  BLOCKED: The server is running in 'non-permissive' mode which has caused this method to be blocked\n", fixed = TRUE)
})

#
# Done
#

context("ds.rep::discctrl::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D"))
})

disconnect.studies.dataset.survival()

context("ds.rep::discctrl::done")
