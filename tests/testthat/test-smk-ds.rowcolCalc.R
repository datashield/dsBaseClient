#-------------------------------------------------------------------------------
# Copyright (c) 2014 OBiBa. All rights reserved.
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

context("dsbaseclient::ds.rowcolCalc")

options(datashield.variables=list("LAB_TSC", "LAB_HDL"))
source("setup.R")

#
# Tests
#

context("dsbaseclient::ds.rowcolCalc()")
opal::datashield.assign(opals, "hdl_tsc", quote(data.frame(cbind(D$LAB_HDL, D$LAB_TSC))))
ds.rowColCalc(x='D', operation="rowSums", newobj="rsum_hdl_tsc")
res <- ds.exists('rsum_hdl_tsc')
test_that("rowColCalc_exists", {
    expect_true(res$sim1)
    expect_true(res$sim2)
    expect_true(res$sim3)
})

context("dsbaseclient::ds.rowcolCalc() no newobj")
opal::datashield.assign(opals, "hdl_tsc", quote(data.frame(cbind(D$LAB_HDL, D$LAB_TSC))))
ds.rowColCalc(x='D', operation="rowSums")
res <- ds.exists('rowColCalc_out')
test_that("rowColCalc_out_exists", {
    expect_true(res$sim1)
    expect_true(res$sim2)
    expect_true(res$sim3)
})

context("dsClient::ds.rowColCalc() test errors")
test_that("rowColCalc_errors", {
    expect_error(ds.rowColCalc(), "Please provide the name of a data.frame or matrix!", fixed=TRUE)
    expect_error(ds.rowColCalc(x='D', newobj="rsum_hdl_tsc"), "'operation' = NULL. Please set it to 'rowSums', 'colSums', 'rowMeans' or 'colMeans'", fixed=TRUE)
    expect_error(ds.rowColCalc(x='D', newobj="rsum_hdl_tsc", operation="datashield"), "'operation' must be set to: 'rowSums', 'colSums', 'rowMeans' or 'colMeans'", fixed=TRUE)
})

#
# Tear down
#

source("teardown.R")