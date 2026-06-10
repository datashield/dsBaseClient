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

# context("ds.vectorCalc::smk::setup")

connect.studies.dataset.cnsim(list('LAB_TSC', 'LAB_HDL'))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.vectorCalc::smk")
test_that("simple test", {
    vectors <- c('D$LAB_TSC', 'D$LAB_HDL')
    res <- expect_warning(ds.vectorCalc(x=vectors, calc='+'), "'ds.vectorCalc' is deprecated.\nUse 'ds.make' instead.", fixed = TRUE)

    expect_length(res, 0)
 
    res_class <- ds.class("vectorcalc.newobj")

    expect_length(res_class, 3)
    expect_length(res_class$sim1, 1)
    expect_equal(res_class$sim1[1], "numeric")
    expect_length(res_class$sim2, 1)
    expect_equal(res_class$sim2[1], "numeric")
    expect_length(res_class$sim3, 1)
    expect_equal(res_class$sim3[1], "numeric")

    res_length <- ds.length("vectorcalc.newobj")

    expect_length(res_length, 4)
    expect_length(res_length$`length of vectorcalc.newobj in sim1`, 1)
    expect_equal(res_length$`length of vectorcalc.newobj in sim1`, 2163)
    expect_length(res_length$`length of vectorcalc.newobj in sim2`, 1)
    expect_equal(res_length$`length of vectorcalc.newobj in sim2`, 3088)
    expect_length(res_length$`length of vectorcalc.newobj in sim3`, 1)
    expect_equal(res_length$`length of vectorcalc.newobj in sim3`, 4128)
    expect_length(res_length$`total length of vectorcalc.newobj in all studies combined`, 1)
    expect_equal(res_length$`total length of vectorcalc.newobj in all studies combined`, 9379)
})

#
# Done
#

# context("ds.vectorCalc::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "vectorcalc.newobj"))
})

disconnect.studies.dataset.cnsim()

# context("ds.vectorCalc::smk::done")
