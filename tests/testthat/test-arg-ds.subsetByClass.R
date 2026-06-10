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

connect.studies.dataset.cnsim(list("LAB_TSC"))

#
# Tests
#

# context("ds.subsetByClass::arg::test errors")
test_that("subsetByClass_erros", {
    expect_error(expect_warning(ds.subsetByClass(), "'ds.subsetByClass' is deprecated.", fixed = TRUE), "Please provide the name of the input data frame or factor!", fixed=TRUE)
    expect_error(expect_warning(ds.subsetByClass(x='D$LAB_TSC'), "'ds.subsetByClass' is deprecated.", fixed = TRUE), "The object to subset from must be a 'data.frame' or a 'factor'.", fixed=TRUE)
})

#
# Tear down
#

disconnect.studies.dataset.cnsim()
