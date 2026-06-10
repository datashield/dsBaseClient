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

# context("ds.rbind::discctrl::setup")

connect.studies.dataset.survival(list("survtime", "time.id", "female", "age.60"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#
# context("ds.rbind::discctrl")
test_that("simple c", {
    expect_error(ds.rbind(c("D$survtime", "D$time.id", "D$female", "D$age.60"), newobj="rbind_newobj"), "There are some DataSHIELD errors, list them with datashield.errors()", fixed = TRUE)

    res_errors <- DSI::datashield.errors()

    expect_length(res_errors, 3)
    expect_length(res_errors$survival1, 1)
    expect_equal(res_errors$survival1, "Command 'rbindDS(\"D$survtime,D$time.id,D$female,D$age.60\", \"D$survtime,D$time.id,D$female,D$age.60\")' failed on 'survival1': Error while evaluating 'is.null(base::assign('rbind_newobj', value={dsBase::rbindDS(\"D$survtime,D$time.id,D$female,D$age.60\", \"D$survtime,D$time.id,D$female,D$age.60\")}))' -> Error in checkPermissivePrivacyControlLevel() : \n  BLOCKED: The server is running in 'non-permissive' mode which has caused this method to be blocked\n", fixed = TRUE)
    expect_length(res_errors$survival2, 1)
    expect_equal(res_errors$survival2, "Command 'rbindDS(\"D$survtime,D$time.id,D$female,D$age.60\", \"D$survtime,D$time.id,D$female,D$age.60\")' failed on 'survival2': Error while evaluating 'is.null(base::assign('rbind_newobj', value={dsBase::rbindDS(\"D$survtime,D$time.id,D$female,D$age.60\", \"D$survtime,D$time.id,D$female,D$age.60\")}))' -> Error in checkPermissivePrivacyControlLevel() : \n  BLOCKED: The server is running in 'non-permissive' mode which has caused this method to be blocked\n", fixed = TRUE)
    expect_length(res_errors$survival3, 1)
    expect_equal(res_errors$survival3, "Command 'rbindDS(\"D$survtime,D$time.id,D$female,D$age.60\", \"D$survtime,D$time.id,D$female,D$age.60\")' failed on 'survival3': Error while evaluating 'is.null(base::assign('rbind_newobj', value={dsBase::rbindDS(\"D$survtime,D$time.id,D$female,D$age.60\", \"D$survtime,D$time.id,D$female,D$age.60\")}))' -> Error in checkPermissivePrivacyControlLevel() : \n  BLOCKED: The server is running in 'non-permissive' mode which has caused this method to be blocked\n", fixed = TRUE)
})

#
# Done
#

# context("ds.rbind::discctrl::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D"))
})

disconnect.studies.dataset.survival()

# context("ds.rbind::discctrl::done")
