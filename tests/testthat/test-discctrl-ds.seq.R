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

# context("ds.seq::discctrl::setup")

connect.studies.dataset.survival(list("survtime", "time.id", "female", "age.60"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#
# context("ds.seq::discctrl")
test_that("simple rep", {
    expect_error(ds.seq(FROM.value.char="1", BY.value.char="1", LENGTH.OUT.value.char="10", ALONG.WITH.name=NULL, newobj="obj1"), "There are some DataSHIELD errors, list them with datashield.errors()", fixed = TRUE)
    
    res_errors <- DSI::datashield.errors()

    expect_length(res_errors, 3)
    expect_length(res_errors$survival1, 1)
    expect_equal(res_errors$survival1, "Command 'seqDS(\"1\", NULL, \"1\", \"10\", NULL)' failed on 'survival1': Error while evaluating 'is.null(base::assign('obj1', value={dsBase::seqDS(\"1\", NULL, \"1\", \"10\", NULL)}))' -> Error in checkPermissivePrivacyControlLevel() : \n  BLOCKED: The server is running in 'non-permissive' mode which has caused this method to be blocked\n", fixed = TRUE)
    expect_length(res_errors$survival2, 1)
    expect_equal(res_errors$survival2, "Command 'seqDS(\"1\", NULL, \"1\", \"10\", NULL)' failed on 'survival2': Error while evaluating 'is.null(base::assign('obj1', value={dsBase::seqDS(\"1\", NULL, \"1\", \"10\", NULL)}))' -> Error in checkPermissivePrivacyControlLevel() : \n  BLOCKED: The server is running in 'non-permissive' mode which has caused this method to be blocked\n", fixed = TRUE)
    expect_length(res_errors$survival3, 1)
    expect_equal(res_errors$survival3, "Command 'seqDS(\"1\", NULL, \"1\", \"10\", NULL)' failed on 'survival3': Error while evaluating 'is.null(base::assign('obj1', value={dsBase::seqDS(\"1\", NULL, \"1\", \"10\", NULL)}))' -> Error in checkPermissivePrivacyControlLevel() : \n  BLOCKED: The server is running in 'non-permissive' mode which has caused this method to be blocked\n", fixed = TRUE)
})

#
# Done
#

# context("ds.seq::discctrl::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D"))
})

disconnect.studies.dataset.survival()

# context("ds.seq::discctrl::done")
