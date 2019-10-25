#-------------------------------------------------------------------------------
# Copyright (c) 2018 University of Newcastle upon Tyne. All rights reserved.
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

connect.studies.dataset.cnsim(list('LAB_TSC', 'LAB_HDL'))

#
# Tests
#

context("ds.vectorCalc::smk")
test_that("simple test", {
    vectors <- c('D$LAB_TSC', 'D$LAB_HDL')
    res <- ds.vectorCalc(x=vectors, calc='+')

    expect_length(res, 0)
 
    res_class <- ds.class("math_output")

    expect_length(res_class, 3)
    expect_length(res_class$sim1, 1)
    expect_equal(res_class$sim1[1], "numeric")
    expect_length(res_class$sim2, 1)
    expect_equal(res_class$sim2[1], "numeric")
    expect_length(res_class$sim3, 1)
    expect_equal(res_class$sim3[1], "numeric")

    res_length <- ds.length("math_output")

    expect_length(res_length, 4)
    expect_length(res_length$`length of math_output in sim1`, 1)
    expect_equal(res_length$`length of math_output in sim1`, 2163)
    expect_length(res_length$`length of math_output in sim2`, 1)
    expect_equal(res_length$`length of math_output in sim2`, 3088)
    expect_length(res_length$`length of math_output in sim3`, 1)
    expect_equal(res_length$`length of math_output in sim3`, 4128)
    expect_length(res_length$`total length of math_output in all studies combined`, 1)
    expect_equal(res_length$`total length of math_output in all studies combined`, 9379)
})

#
# Done
#

disconnect.studies.dataset.cnsim()
