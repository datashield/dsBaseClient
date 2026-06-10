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

connect.studies.dataset.cnsim(list("LAB_TSC", "LAB_HDL"))

#
# Tests
#

# context("ds.rowColCalc::arg::test errors")
test_that("rowColCalc_errors", {
    expect_error(ds.rowColCalc(), "Please provide the name of a data.frame or matrix!", fixed=TRUE)
    expect_error(ds.rowColCalc(x='D', newobj="rsum_hdl_tsc"), "'operation' = NULL. Please set it to 'rowSums', 'colSums', 'rowMeans' or 'colMeans'", fixed=TRUE)
    expect_error(ds.rowColCalc(x='D', newobj="rsum_hdl_tsc", operation="datashield"), "'operation' must be set to: 'rowSums', 'colSums', 'rowMeans' or 'colMeans'", fixed=TRUE)
})

#
# Tear down
#

disconnect.studies.dataset.cnsim()
