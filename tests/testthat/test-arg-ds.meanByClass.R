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

connect.studies.dataset.cnsim(list("LAB_TSC","LAB_HDL","GENDER","DIS_DIAB","PM_BMI_CATEGORICAL"))

#
# Tests
#

# context("ds.meanByClass::arg::test errors")
test_that("meanByClass_erros", {
    ds.asCharacter(x='D$GENDER', newobj="not_a_numeric")
    ds.asCharacter(x='D$GENDER', newobj="not_a_factor")
    ds.assign("D$GENDER", "sex")
    ds.assign("D$LAB_HDL", "ldl")

    expect_error(expect_warning(ds.meanByClass(), "'ds.meanByClass' is deprecated.", fixed = TRUE), "Please provide the name data frame or matrix or a formula of the form 'A~B' where A is a continuous vector and B a factor vector!", fixed=TRUE)
    expect_error(expect_warning(ds.meanByClass(x='not_a_numeric~sex'), "'ds.meanByClass' is deprecated.", fixed = TRUE), "The first element in the formula must be of type numeric or integer!", fixed=TRUE)
    expect_error(expect_warning(ds.meanByClass(x='ldl~not_a_factor'), "'ds.meanByClass' is deprecated.", fixed = TRUE), "The second element in the formula must be of type factor!", fixed=TRUE)
    expect_error(expect_warning(ds.meanByClass(x='ldl~sex~not_a_factor'), "'ds.meanByClass' is deprecated.", fixed = TRUE), "x must be the name of a data frame or a matrix or a formula of the form 'A~B' where A is a continuous vector and B a factor vector!", fixed=TRUE)
})

#
# Tear down
#

disconnect.studies.dataset.cnsim()
