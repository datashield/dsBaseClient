#-------------------------------------------------------------------------------
# Copyright (c) 2019-2021 University of Newcastle upon Tyne. All rights reserved.
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

connect.studies.dataset.cnsim(list("LAB_TSC"))

#
# Tests
#

context("ds.subsetByClass::arg::test errors")
test_that("subsetByClass_erros", {
    expect_error(ds.subsetByClass(), "Please provide the name of the input data frame or factor!", fixed=TRUE)
    expect_error(ds.subsetByClass(x='D$LAB_TSC'), "The object to subset from must be a 'data.frame' or a 'factor'.", fixed=TRUE)
})

#
# Tear down
#

disconnect.studies.dataset.cnsim()
