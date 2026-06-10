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

# context("ds.replaceNA::smk::setup")

connect.studies.dataset.cnsim(list("LAB_HDL", "LAB_TRIG", "DIS_CVA"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.replaceNA::smk")
test_that("simple replaceNA", {
    res1 <- ds.replaceNA(x='D$LAB_HDL', forNA=c(1.0, 2.0, 3.0), newobj="newobj1")
    expect_length(res1, 0)

    res2 <- ds.replaceNA(x='D$LAB_TRIG', forNA=c(1.0, 2.0, 3.0), newobj="newobj2")
    expect_length(res2, 0)

    res3 <- ds.replaceNA(x='D$DIS_CVA', forNA=c(1.0, 2.0, 3.0), newobj="newobj3")
    expect_length(res3, 0)
})

#
# Done
#

# context("ds.replaceNA::smk::shutdown")

test_that("setup", {
    ds_expect_variables(c("D", "newobj1", "newobj2", "newobj3"))
})

disconnect.studies.dataset.cnsim()

# context("ds.replaceNA::smk::done")
