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

connect.studies.dataset.cnsim(list("LAB_TSC","LAB_HDL","GENDER","DIS_DIAB","PM_BMI_CATEGORICAL"))

#
# Tests
#

context("ds.meanByClass::arg::test errors")
ds.asCharacter(x='D$GENDER', newobj="not_a_numeric")
ds.asCharacter(x='D$GENDER', newobj="not_a_factor")
ds.assign("D$GENDER", "sex")
ds.assign("D$LAB_HDL", "ldl")
test_that("meanByClass_erros", {
    expect_error(ds.meanByClass(), "Please provide the name data frame or matrix or a formula of the form 'A~B' where A is a continuous vector and B a factor vector!", fixed=TRUE)
    expect_error(ds.meanByClass(x='not_a_numeric~sex'), "The first element in the formula must be of type numeric or integer!", fixed=TRUE)
    expect_error(ds.meanByClass(x='ldl~not_a_factor'), "The second element in the formula must be of type factor!", fixed=TRUE)
    expect_error(ds.meanByClass(x='ldl~sex~not_a_factor'), "x must be the name of a data frame or a matrix or a formula of the form 'A~B' where A is a continuous vector and B a factor vector!", fixed=TRUE)
})

#
# Tear down
#

disconnect.studies.dataset.cnsim()
