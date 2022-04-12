#-------------------------------------------------------------------------------
# Copyright (c) 2019-2022 University of Newcastle upon Tyne. All rights reserved.
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

context("ds.ns::discctrl::setup")

connect.studies.dataset.cnsim(list("LAB_TRIG", "PM_BMI_CONTINUOUS"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#
context("ds.ns::discctrl")
test_that("ns", {
    expect_error(ds.ns(x="D$PM_BMI_CONTINUOUS", knots=c(8,9,10,12,25,35), newobj="nsDS", datasources=connections), 
                 "There are some DataSHIELD errors, list them with datashield.errors()", fixed = TRUE)
    
    res_errors <- DSI::datashield.errors()

    expect_length(res_errors, 1)
    expect_length(res_errors$study3, 1)
    expect_equal(res_errors$study3, "Command 'nsDS(\"D$PM_BMI_CONTINUOUS\", NULL, c(8, 9, 10, 12, 25, 35), FALSE, \n    NULL)' failed on 'study3': Error while evaluating 'is.null(base::assign('nsDS', value={.ASSIGN$nsDS(\"D$PM_BMI_CONTINUOUS\", NULL, base::c(8, 9, 10, 12, 25, 35), FALSE, NULL)}))' -> Error : One of the spline segments has less than 3 observations. Please redefine the knot positions\n")
})

#
# Done
#

context("ds.ns::discctrl::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D"))
})

disconnect.studies.dataset.cnsim()

context("ds.ns::discctrl::done")
