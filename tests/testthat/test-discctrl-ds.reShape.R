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

# context("ds.reShape::discctrl::setup")

connect.studies.dataset.survival(list("id", "study.id", "time.id", "cens", "age.60", "female"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#
# context("ds.reShape::discctrl")
test_that("simple reShape", {
    expect_error(ds.reShape(data.name="D", v.names="age.60", timevar.name="time.id", idvar.name="id", direction="wide", newobj="reshape1_obj"), "There are some DataSHIELD errors, list them with datashield.errors()", fixed = TRUE)
    
    res_errors <- DSI::datashield.errors()

    expect_length(res_errors, 3)
    expect_length(res_errors$survival1, 1)
    expect_equal(res_errors$survival1, "Command 'reShapeDS(\"D\", NULL, \"age.60\", \"time.id\", \"id\", NULL, \"wide\", \n    \".\")' failed on 'survival1': Error while evaluating 'is.null(base::assign('reshape1_obj', value={dsBase::reShapeDS(\"D\", NULL, \"age.60\", \"time.id\", \"id\", NULL, \"wide\", \".\")}))' -> Error in checkPermissivePrivacyControlLevel() : \n  BLOCKED: The server is running in 'non-permissive' mode which has caused this method to be blocked\n", fixed = TRUE)
    expect_length(res_errors$survival2, 1)
    expect_equal(res_errors$survival2, "Command 'reShapeDS(\"D\", NULL, \"age.60\", \"time.id\", \"id\", NULL, \"wide\", \n    \".\")' failed on 'survival2': Error while evaluating 'is.null(base::assign('reshape1_obj', value={dsBase::reShapeDS(\"D\", NULL, \"age.60\", \"time.id\", \"id\", NULL, \"wide\", \".\")}))' -> Error in checkPermissivePrivacyControlLevel() : \n  BLOCKED: The server is running in 'non-permissive' mode which has caused this method to be blocked\n", fixed = TRUE)
    expect_length(res_errors$survival3, 1)
    expect_equal(res_errors$survival3, "Command 'reShapeDS(\"D\", NULL, \"age.60\", \"time.id\", \"id\", NULL, \"wide\", \n    \".\")' failed on 'survival3': Error while evaluating 'is.null(base::assign('reshape1_obj', value={dsBase::reShapeDS(\"D\", NULL, \"age.60\", \"time.id\", \"id\", NULL, \"wide\", \".\")}))' -> Error in checkPermissivePrivacyControlLevel() : \n  BLOCKED: The server is running in 'non-permissive' mode which has caused this method to be blocked\n", fixed = TRUE)
})

#
# Done
#

# context("ds.reShape::discctrl::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D"))
})

disconnect.studies.dataset.survival()

# context("ds.reShape::discctrl::done")
